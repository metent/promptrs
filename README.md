# promptrs

A lightweight engine for creating and running agentic workflows using Deno's V8 sandbox. It allows you to define AI agents that interact with the filesystem and network, and execute other processes in a constrained environment. The agents can communicate with the engine and optionally, with other agents by transferring structured messages via standard input and output.

## Features
- **Sandboxed Execution**: Run agent scripts in a restricted environment.
- **LLM Integration**: Communicate with LLMs using custom workflows.
- **Tool Support**: Define and use tools for file manipulation, system commands, etc.
- **Modular Design**: Agents can be extended with new functionality.

## Usage
```bash
promptrs run <agent-name> --dir <sandbox-directory> --allow-run <command>
```

### Example: Run a todo agent
```bash
promptrs run todo_agent --dir ./todo_project --allow-run cargo
```

## Agent Definition (TypeScript)
Agents are defined in TypeScript files and use a specific format to structure their interactions with the system, tools, and LLM responses.
