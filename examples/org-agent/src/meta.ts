export class Tools {
  tools: Record<string, (_: never) => string> = {};
  oaiSpec: {
    name: string;
    arguments: {
      "$schema": string;
      description: string;
      properties: Record<string, { type: string; description: string }>;
    };
  }[] = [];
}

export function tool(description: string, parameters: Record<string, string>) {
  return <T extends Tools, Args>(
    originalMethod: (args: Args) => string,
    context: ClassMethodDecoratorContext<T, (args: Args) => string>,
  ) => {
    context.addInitializer(function () {
      this.tools[context.name.toString()] = originalMethod;
      this.oaiSpec.push({
        name: context.name.toString(),
        arguments: {
          "$schema": "https://json-schema.org/draft/2020-12/schema",
          description,
          properties: Object.fromEntries(
            Object.entries(parameters).map(([name, description]) => [name, {
              type: "string",
              description,
            }]),
          ),
        },
      });
    });
    return originalMethod;
  };
}
