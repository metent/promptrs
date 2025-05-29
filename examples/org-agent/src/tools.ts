import {
  descriptor,
  readdirSync,
  readFileSync,
  rmSync,
  sleep,
  writeFileSync,
} from "./wasi.ts";
import {
  calculateInsertPosition,
  findNewParentInfo,
  findTaskEnd,
  findTaskHeadingIndex,
  processProperties,
} from "./util.ts";
import { tool, Tools } from "./meta.ts";

export class OrgTools extends Tools {
  timezone?: string;

  @tool("Adds a new task/subtask to the org file", {
    id: "Task ID (required)",
    title: "Task title (required)",
    parentId: "ID of parent task or 'root' for top-level tasks",
    type: "Type ('TASK' or 'NOTE')",
    priority: "Priority letter (A-H) (optional)",
    scheduledDate: "Scheduled date in org-mode date format (optional)",
    dueDate: "Due date in org-mode date format (optional)",
  })
  addTask({
    id,
    title,
    parentId = "root",
    type = "TASK",
    priority,
    scheduledDate,
    dueDate,
  }: Record<string, string>) {
    try {
      const content = readFileSync("knowledge.org", "utf8");
      const lines = content.split("\n");

      const taskId = id;

      let parentPos = -1;
      let indentLevel = 0;
      let endOfParentProps = -1;

      if (parentId !== "root") {
        for (let i = 0; i < lines.length; i++) {
          const headingMatch = lines[i].match(/^(\*+)\s*/);
          if (!headingMatch) continue;

          let propsStart = -1;
          for (let j = i + 1; j < lines.length; j++) {
            if (lines[j].match(/^:PROPERTIES:/)) {
              propsStart = j;
              break;
            }
          }

          if (propsStart === -1) continue;

          let foundId = false;
          for (let k = propsStart + 1; k < lines.length; k++) {
            if (lines[k].match(/^:END:/)) break;
            if (lines[k].includes(`:ID: ${parentId}`)) {
              foundId = true;
              parentPos = i;
              indentLevel = headingMatch[1].length;
              endOfParentProps = k + 1;
              break;
            }
          }

          if (foundId) {
            while (
              endOfParentProps < lines.length &&
              !lines[endOfParentProps].match(/^:END:/)
            ) {
              endOfParentProps++;
            }
            if (endOfParentProps >= lines.length) {
              return `Invalid parent PROPERTIES block for ID ${parentId}`;
            }
            break;
          }
        }

        if (parentPos === -1) {
          return `Parent task with ID ${parentId} not found`;
        }
      } else {
        endOfParentProps = lines.length;
      }

      const newTaskIndent = "*".repeat(indentLevel + 1);
      const newTaskLines = [
        `${newTaskIndent} ${title}`,
        ":PROPERTIES:",
        `:ID: ${taskId}`,
        type === "TASK" ? ":TYPE: TASK" : ":TYPE: NOTE",
        ...priority ? [`:PRIORITY: ${priority}`] : [],
        ...dueDate ? [`:DEADLINE: ${dueDate}`] : [],
        ...scheduledDate ? [`:SCHEDULED: ${scheduledDate}`] : [],
        ":END:",
      ];

      let insertPos;
      if (parentId !== "root") {
        insertPos = endOfParentProps + 1;
      } else {
        insertPos = lines.length;
      }

      if (lines.length === 0 || (lines.length === 1 && lines[0] === "")) {
        insertPos = 0;
      }

      lines.splice(insertPos, 0, ...newTaskLines);

      writeFileSync(
        "knowledge.org",
        lines.join("\n").trim() + "\n",
      );

      return `Created task "${title}" with ID ${taskId}`;
    } catch (err) {
      return `Error: ${JSON.stringify({ error: err })}`;
    }
  }

  @tool("Updates task properties via PATCH-like operation", {
    id: "Unique task ID to update",
    priority: "Priority letter (A-H) (optional)",
    scheduledDate: "Scheduled date in org-mode date format (optional)",
    dueDate: "Due date in org-mode date format (optional)",
  })
  updateTask({
    id,
    priority,
    dueDate,
    scheduledDate,
  }: Record<string, string>) {
    try {
      const content = readFileSync("knowledge.org", "utf8");
      const lines = content.split("\n");
      let taskPos = -1;

      // Find the task line with :ID: id
      for (let i = 0; i < lines.length; i++) {
        if (lines[i].includes(`:ID: ${id}`)) {
          taskPos = i;
          break;
        }
      }

      if (taskPos === -1) {
        return `Task not found`;
      }

      // Collect all lines from taskPos until :END: line (inclusive)
      const taskLines = [];
      let endIndex = -1;
      for (let i = taskPos; i < lines.length; i++) {
        taskLines.push(lines[i]);
        if (lines[i].trim().startsWith(":END:")) {
          endIndex = i;
          break;
        }
      }

      // Check if :END: was found
      if (endIndex === -1) {
        return `Task missing :END: line`;
      }

      // Process the task lines to update properties
      processProperties(taskLines, priority, dueDate, scheduledDate);

      // Replace the original lines with the modified taskLines
      // The original slice is from taskPos to endIndex (inclusive)
      const numLinesToRemove = endIndex - taskPos + 1;
      lines.splice(taskPos, numLinesToRemove, ...taskLines);

      // Write back the updated content
      writeFileSync("knowledge.org", lines.join("\n"));
      return `Updated task ${id}`;
    } catch (err) {
      return `${JSON.stringify({ error: err })}`;
    }
  }

  @tool("Removes a task and all subtasks", {
    id: "Unique task ID to remove",
  })
  removeTask({ id }: Record<string, string>) {
    try {
      const content = readFileSync("knowledge.org", "utf8");
      const lines = content.split("\n");

      // Find the task's heading index and ID line
      const taskInfo = findTaskHeadingIndex(lines, id);
      if (!taskInfo) {
        return `Task ${id} not found`;
      }
      const [headingIdx] = taskInfo;

      // Determine where the task ends (exclusive)
      const endLine = findTaskEnd(lines, headingIdx);

      // Calculate the number of lines to remove
      const linesToRemove = endLine - headingIdx;

      // Remove the task and its subtree from the lines array
      lines.splice(headingIdx, linesToRemove);

      // Save the modified content back to the file
      writeFileSync("knowledge.org", lines.join("\n"));

      return `Task ${id} deleted successfully`;
    } catch (err) {
      return `${JSON.stringify({ error: err })}`;
    }
  }

  @tool("Moves a task under a new parent", {
    id: "ID of task to move",
    newParentId: "New parent task ID or 'root'",
    position: "Position relative to siblings ('first', 'last', index number)",
  })
  moveTask({
    id,
    newParentId,
    position,
  }: { id: string; newParentId: string; position: number | "first" | "last" }) {
    try {
      const content = readFileSync("knowledge.org", "utf8");
      const lines = content.split("\n");

      // Find the task's heading index and ID line
      const taskInfo = findTaskHeadingIndex(lines, id);
      if (!taskInfo) {
        return `Task ${id} not found`;
      }
      const [headingIdx] = taskInfo;

      // Determine where the task ends (exclusive)
      const endLine = findTaskEnd(lines, headingIdx);

      // Extract original task block
      const originalBlock = lines.slice(headingIdx, endLine);

      // Get current indentation level of the task's heading
      const currentIndentLevel = (lines[headingIdx].match(/^(\*+)/))![1].length;

      // Validate new parent exists (if not root)
      const newParentInfo = findNewParentInfo(lines, newParentId);
      if (!newParentInfo && newParentId !== "root") {
        return `New parent ${newParentId} not found`;
      }

      // Calculate new indent level for the task's heading under new parent
      const newIndentLevel = newParentId === "root"
        ? 1
        : (newParentInfo!.indentLevel + 1);

      // Adjust block indentation (same as copyTask)
      const adjustedBlock = originalBlock.map((line) => {
        const starMatch = line.match(/^(\*+)/);
        if (!starMatch) return line;

        const originalStars = starMatch[1].length;
        // Only process lines that are part of the task's subtree (indent >= heading indent)
        if (originalStars < currentIndentLevel) return line;

        const depth = originalStars - currentIndentLevel;
        const newStarCount = newIndentLevel + depth;

        return line.replace(starMatch[1], "*".repeat(newStarCount));
      });

      // Remove the original task block from lines
      const linesToRemove = endLine - headingIdx;
      lines.splice(headingIdx, linesToRemove);

      // Calculate insert position in the new location
      const insertPos = calculateInsertPosition(
        lines,
        newParentId,
        newParentId === "root" ? null : newParentInfo,
        position,
      );

      // Insert the adjusted block at the calculated position
      lines.splice(insertPos, 0, ...adjustedBlock);

      // Save the modified content
      writeFileSync("knowledge.org", lines.join("\n"));

      return `Task ${id} moved successfully`;
    } catch (error) {
      console.error("Move task error:", error);
      return `Error: ${String(error)}`;
    }
  }

  @tool("Copies a task with all subtasks under new parent", {
    id: "ID of task to copy",
    newParentId: "New parent task ID or 'root'",
    position: "Position relative to siblings ('first', 'last', index number)",
  })
  copyTask({
    id,
    newParentId = "root",
    position = "last",
  }: { id: string; newParentId: string; position: number | "first" | "last" }) {
    try {
      if (position !== "first" && position !== "last") {
        position = Number(position);
      }
      const content = readFileSync("knowledge.org", "utf8");
      const lines = content.split("\n");

      // Find original task
      const taskInfo = findTaskHeadingIndex(lines, id);
      if (!taskInfo) {
        return `Task ${id} not found`;
      }
      const [headingIdx, _idLine] = taskInfo;

      // Get task block
      const taskEnd = findTaskEnd(lines, headingIdx);
      const originalBlock = lines.slice(headingIdx, taskEnd);

      // Generate new ID and adjust block
      const newId = `copy-${id}-${Date.now()}`;
      const currentIndentLevel = (lines[headingIdx].match(/^(\*+)/))![1].length;
      const parentInfo = findNewParentInfo(lines, newParentId);
      if (!parentInfo && newParentId !== "root") {
        return `New parent ${newParentId} not found`;
      }

      // Calculate new indentation level for the heading
      const newIndentLevel = newParentId === "root"
        ? 1
        : (parentInfo!.indentLevel + 1);

      // Adjust block indentation with depth preservation
      const adjustedBlock = originalBlock.map((line) => {
        const starMatch = line.match(/^(\*+)/);
        if (!starMatch) return line; // Non-headline lines

        const originalStars = starMatch[1].length;
        // Only process lines that are part of the task's subtree (indent >= heading indent)
        if (originalStars < currentIndentLevel) return line;

        // Calculate depth relative to original heading
        const depth = originalStars - currentIndentLevel;
        // Compute new indentation based on target parent and depth
        const newStarCount = newIndentLevel + depth;

        // Replace the stars with new count while preserving rest of the line
        return line.replace(starMatch[1], "*".repeat(newStarCount));
      });

      // Update ID in the new block
      const idLineInBlock = adjustedBlock.findIndex((l) =>
        l.includes(`:ID: ${id}`)
      );
      if (idLineInBlock !== -1) {
        adjustedBlock[idLineInBlock] = adjustedBlock[idLineInBlock].replace(
          `:ID: ${id}`,
          `:ID: ${newId}`,
        );
      }

      // Calculate insertion position
      const insertPos = calculateInsertPosition(
        lines,
        newParentId,
        newParentId === "root" ? null : parentInfo,
        position,
      );

      // Insert and save
      lines.splice(insertPos, 0, ...adjustedBlock);
      writeFileSync("knowledge.org", lines.join("\n"));
      return `Task copied. New ID: ${newId}`;
    } catch (err) {
      return `Error copying task: ${JSON.stringify({ error: err })}`;
    }
  }

  promptUser(context: string) {
    writeFileSync(
      "knowledge.prompt",
      context.replaceAll(/^(.*)$/gm, "> $1"),
    );
    let response = "";
    while (response === "") {
      sleep(5n);
      const content = readFileSync("knowledge.prompt", "utf8");
      response = content.replaceAll(/^(>.*\n?)*\n?/g, "").trim();
    }
    return response;
  }

  processInstructions(context: string) {
    const [entry, fd] = readdirSync("", descriptor());

    if (entry) {
      try {
        const input = readFileSync(entry, "utf8", fd).trim();
        rmSync(entry);
        return input;
      } catch (_err) {
        _err;
      }
    }

    return this.promptUser(context);
  }

  getStatus() {
    const text = readFileSync("knowledge.org", "utf8");
    return `Current time: ${
      new Date().toLocaleString("en-GB", { timeZone: this.timezone })
    }\nThe current contents are:\n${text}`;
  }
}
