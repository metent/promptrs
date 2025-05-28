// Helper function to process properties in taskLines
export function processProperties(
  taskLines: string[],
  priority?: string,
  dueDate?: string,
  scheduledDate?: string,
) {
  // Process PRIORITY
  if (priority !== undefined) {
    let found = false;
    for (let i = 0; i < taskLines.length; i++) {
      const line = taskLines[i].trim();
      if (line.startsWith(":PRIORITY:")) {
        taskLines[i] = `:PRIORITY: ${priority}`;
        found = true;
        break;
      }
    }
    if (!found) {
      // Find the index of :END:
      const endIdx = taskLines.findIndex((line) =>
        line.trim().startsWith(":END:")
      );
      if (endIdx !== -1) {
        // Insert before :END:
        taskLines.splice(endIdx, 0, `:PRIORITY: ${priority}`);
      } else {
        // If for some reason :END: is not found (shouldn't happen), append
        taskLines.push(`:PRIORITY: ${priority}`);
      }
    }
  }

  // Process DUE DATE (:DEADLINE:)
  if (dueDate !== undefined) {
    let found = false;
    for (let i = 0; i < taskLines.length; i++) {
      const line = taskLines[i].trim();
      if (line.startsWith(":DEADLINE:")) {
        taskLines[i] = `:DEADLINE: <${dueDate}>`;
        found = true;
        break;
      }
    }
    if (!found) {
      const endIdx = taskLines.findIndex((line) =>
        line.trim().startsWith(":END:")
      );
      if (endIdx !== -1) {
        taskLines.splice(endIdx, 0, `:DEADLINE: <${dueDate}>`);
      } else {
        taskLines.push(`:DEADLINE: <${dueDate}>`);
      }
    }
  }

  // Process SCHEDULED DATE
  if (scheduledDate !== undefined) {
    let found = false;
    for (let i = 0; i < taskLines.length; i++) {
      const line = taskLines[i].trim();
      if (line.startsWith(":SCHEDULED:")) {
        taskLines[i] = `:SCHEDULED: <${scheduledDate}>`;
        found = true;
        break;
      }
    }
    if (!found) {
      const endIdx = taskLines.findIndex((line) =>
        line.trim().startsWith(":END:")
      );
      if (endIdx !== -1) {
        taskLines.splice(endIdx, 0, `:SCHEDULED: <${scheduledDate}>`);
      } else {
        taskLines.push(`:SCHEDULED: <${scheduledDate}>`);
      }
    }
  }
}

// Keep helper functions unchanged unless needed
export function findTaskHeadingIndex(
  lines: string[],
  id: string,
) {
  let taskIdLine = -1;
  for (let i = 0; i < lines.length; i++) {
    if (lines[i].includes(`:ID: ${id}`)) {
      taskIdLine = i;
      break;
    }
  }
  if (taskIdLine === -1) return null;

  let headingIndex = taskIdLine - 1;
  while (headingIndex >= 0 && !lines[headingIndex].match(/^(\*+)\s*/)) {
    headingIndex--;
  }
  if (headingIndex < 0) return null;

  return [headingIndex, taskIdLine];
}

export function findTaskEnd(lines: string[], startLine: number) {
  const currentIndent = (lines[startLine].match(/^(\*+)/))![1].length;
  let endLine = startLine + 1;
  while (endLine < lines.length) {
    const line = lines[endLine];
    const indentMatch = line.match(/^(\*+)/);
    if (indentMatch && indentMatch[1].length <= currentIndent) break;
    endLine++;
  }
  return endLine;
}

export function findNewParentInfo(
  lines: string[],
  newParentId: string,
) {
  if (newParentId === "root") return { headingIndex: -1, indentLevel: 0 };

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (!line.match(/^(\*+)\s*/)) continue;

    let propsStart = -1;
    let j = i + 1;
    while (j < lines.length && !lines[j].includes(":PROPERTIES:")) {
      if (/^\s*$/.test(lines[j]) || lines[j].startsWith("#+")) {
        j++;
        continue;
      }
      break;
    }

    if (j >= lines.length) continue;

    propsStart = j + 1;
    let hasId = false;
    while (
      propsStart < lines.length && !lines[propsStart].includes(":END:")
    ) {
      if (lines[propsStart].includes(`:ID: ${newParentId}`)) {
        hasId = true;
        break;
      }
      propsStart++;
    }
    if (hasId) {
      const indentLevel = (line.match(/^(\*+)/))![1].length;
      return { headingIndex: i, indentLevel };
    }
  }
  return null;
}

export function calculateInsertPosition(
  lines: string[],
  newParentId: string,
  newParentInfo: { headingIndex: number; indentLevel: number } | null,
  position: "first" | "last" | number,
) {
  if (position !== "first" && position !== "last") {
    position = Number(position);
  }
  if (newParentId === "root") {
    const topLevel = [];
    for (let i = 0; i < lines.length; i++) {
      const m = lines[i].match(/^(\*+)/);
      if (m && m[1].length === 1) topLevel.push(i);
    }

    if (typeof position === "number") {
      return Math.max(0, Math.min(position, topLevel.length));
    }
    return position === "last"
      ? (topLevel.length > 0 ? topLevel[topLevel.length - 1] + 1 : 0)
      : 0;
  } else {
    const { headingIndex: parentIdx, indentLevel: parentIndent } =
      newParentInfo!;

    let parentEnd = lines.length;
    let currentLine = parentIdx + 1;
    while (currentLine < lines.length) {
      const line = lines[currentLine];
      const indentMatch = line.match(/^(\*+)/);
      if (!indentMatch) {
        currentLine++;
        continue;
      }
      const currentIndentLevel = indentMatch[1].length;
      if (currentIndentLevel <= parentIndent) {
        parentEnd = currentLine;
        break;
      }
      currentLine++;
    }

    const contentStart = parentIdx + 1;
    const contentEnd = parentEnd - 1;

    let bodyStart = contentStart;
    for (let i = contentStart; i <= contentEnd; i++) {
      if (lines[i].includes(":PROPERTIES:")) {
        let propsEnd = i + 1;
        while (
          propsEnd <= contentEnd && !lines[propsEnd].includes(":END:")
        ) {
          propsEnd++;
        }
        bodyStart = propsEnd + 1;
        break;
      }
    }

    const children = [];
    for (let i = bodyStart; i <= contentEnd; i++) {
      const lineIndentMatch = lines[i].match(/^(\*+)/);
      if (!lineIndentMatch) continue;
      const lineIndent = lineIndentMatch[1].length;
      if (lineIndent === parentIndent + 1) {
        children.push(i);
      }
    }

    if (typeof position === "number") {
      return Math.max(0, Math.min(position, children.length));
    }
    return position === "last"
      ? (children.length > 0 ? contentEnd + 1 : bodyStart)
      : (children.length > 0 ? children[0] : bodyStart);
  }
}
