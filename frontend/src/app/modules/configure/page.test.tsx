import { enableFetchMocks, MockResponseInit } from "jest-fetch-mock";
enableFetchMocks();

import endent from "endent";
import userEvent, { UserEvent } from "@testing-library/user-event";
import "@testing-library/jest-dom";
import { act, render, screen, within } from "@testing-library/react";
import { AgeWasm } from "@/age-wasm-compiled";
import { match, P } from "ts-pattern";
import { AvailableModulesReply, ModuleSchema } from "@/services/modules";
import { Entry } from "./page";
import { _normalize } from "./moduleInputs/nixPath";

let searchParams = new URLSearchParams();
jest.mock("next/navigation", () => ({
  useRouter: jest.fn(),
  useSearchParams: () => searchParams,
}));

jest.mock("../../../age-wasm-compiled/index", () => ({
  initAgeWasm: async (): Promise<AgeWasm> => ({
    encrypt(publicKey, secret) {
      return { ok: true, data: `<MOCK_ENCRYPTED_FOR ${publicKey} ${secret}>` };
    },
  }),
}));

let savedConfig: null | unknown = null;
let user: UserEvent = userEvent.setup();
let availableModules: AvailableModulesReply = { modules: [] };

const setupTestModuleSchema = (
  fields: Record<string, ModuleSchema>,
  options: { description?: string } = {},
): void => {
  availableModules = {
    modules: [
      {
        description: options.description || null,
        git_commit: "aaa",
        name: "Test",
        repo_name: "test-module",
        repo_user: "garnix-io",
        schema: {
          description: options.description || undefined,
          typ: {
            fields: {
              testModule: {
                typ: {
                  fieldType: {
                    fields,
                    tag: "submodule",
                  },
                  tag: "attrsOf",
                },
              },
            },
            tag: "submodule",
          },
        },
      },
    ],
  };
};

const setupApiMocks = () => {
  availableModules = {
    modules: [
      {
        description: "A garnix module for nodejs",
        git_commit: "c534ff50b006663651d6445e3c55e76da7fd9947",
        name: "NodeJS",
        repo_name: "nodejs-module",
        repo_user: "garnix-io",
        schema: {
          description: "A garnix module for nodejs",
          typ: {
            fields: {
              nodejs: {
                description: "An attrset of nodejs projects to generate",
                typ: {
                  fieldType: {
                    fields: {
                      buildDependencies: {
                        default: { tag: "list", value: [] },
                        description:
                          "A list of dependencies required to build this package. They are made available in the devshell, and at build time",
                        typ: {
                          elementType: { tag: "package" },
                          tag: "listOf",
                        },
                      },
                      devTools: {
                        default: { tag: "list", value: [] },
                        description:
                          "A list of packages make available in the devshell for this project. This is useful for things like LSPs, formatters, etc.",
                        typ: {
                          elementType: { tag: "package" },
                          tag: "listOf",
                        },
                      },
                      prettier: {
                        default: { tag: "bool", value: false },
                        description:
                          "Whether to create a CI check with prettier, and add it to the devshells",
                        typ: { tag: "bool" },
                      },
                      runtimeDependencies: {
                        default: { tag: "list", value: [] },
                        description:
                          "A list of dependencies required at runtime. They are made available in the devshell, at build time, and are available on the server at runtime",
                        typ: {
                          elementType: { tag: "package" },
                          tag: "listOf",
                        },
                      },
                      src: {
                        description:
                          "A path to the directory containing package.json, package.lock, and src",
                        example: "./.",
                        typ: { tag: "path" },
                      },
                      testCommand: {
                        default: { tag: "string", value: "npm run test" },
                        description: "The command to run the test.",
                        typ: { tag: "str" },
                      },
                      webServer: {
                        default: { tag: "null" },
                        description:
                          "Whether to create an HTTP server based on this NodeJS project",
                        typ: {
                          innerType: {
                            fields: {
                              command: {
                                description:
                                  "The command to run to start the server in production",
                                example: "server --port 7000",
                                typ: { tag: "nonEmptyStr" },
                              },
                              path: {
                                default: { tag: "string", value: "/" },
                                description:
                                  "Path to host your nodejs server on",
                                typ: { tag: "nonEmptyStr" },
                              },
                              port: {
                                default: { tag: "int", value: 3000 },
                                description:
                                  "Port to forward incoming http requests to",
                                typ: { tag: "unsignedInt16" },
                              },
                            },
                            tag: "submodule",
                          },
                          tag: "nullOr",
                        },
                      },
                    },
                    tag: "submodule",
                  },
                  tag: "attrsOf",
                },
              },
            },
            tag: "submodule",
          },
        },
      },
      {
        description: "A garnix module for postgreSQL",
        git_commit: "c534ff50b006663651d6445e3c55e76da7fd9947",
        name: "PostgreSQL",
        repo_name: "postgresql-module",
        repo_user: "garnix-io",
        schema: {
          description: "A garnix module for postgreSQL",
          typ: {
            fields: {
              postgresql: {
                description: "An attrset of postgresql databases",
                typ: {
                  fieldType: {
                    fields: {
                      port: {
                        default: { tag: "int", value: 5432 },
                        description: "The port in which to run PostgreSQL",
                        typ: { tag: "unsignedInt16" },
                      },
                    },
                    tag: "submodule",
                  },
                  tag: "attrsOf",
                },
              },
            },
            tag: "submodule",
          },
        },
      },
      {
        description: null,
        git_commit: "c534ff50b006663651d6445e3c55e76da7fd9947",
        name: "Rust",
        repo_name: "rust-module",
        repo_user: "garnix-io",
        schema: {
          typ: {
            fields: {
              rust: {
                description: "An attrset of rust projects to generate",
                typ: {
                  fieldType: {
                    fields: {
                      buildDependencies: {
                        default: { tag: "list", value: [] },
                        description:
                          "A list of dependencies required to build this package. They are made available in the devshell, and at build time",
                        name: "build dependencies",
                        typ: {
                          elementType: { tag: "package" },
                          tag: "listOf",
                        },
                      },
                      devTools: {
                        default: { tag: "list", value: [] },
                        description:
                          "A list of packages make available in the devshell for this project (and `default` devshell). This is useful for things like LSPs, formatters, etc.",
                        name: "development tools",
                        typ: {
                          elementType: { tag: "package" },
                          tag: "listOf",
                        },
                      },
                      runtimeDependencies: {
                        default: { tag: "list", value: [] },
                        description:
                          "A list of dependencies required at runtime. They are made available in the devshell, at build time, and are available on the server at runtime",
                        name: "runtime dependencies",
                        typ: {
                          elementType: { tag: "package" },
                          tag: "listOf",
                        },
                      },
                      src: {
                        description:
                          "A path to the directory containing Cargo.lock, Cargo.toml, and src",
                        example: "./.",
                        name: "source directory",
                        typ: { tag: "path" },
                      },
                      webServer: {
                        default: { tag: "null" },
                        description:
                          "Whether to create an HTTP server based on this Rust project",
                        typ: {
                          innerType: {
                            fields: {
                              command: {
                                description:
                                  "The command to run to start the server in production",
                                example: "server --port 7000",
                                name: "server command",
                                typ: { tag: "nonEmptyStr" },
                              },
                              path: {
                                default: { tag: "string", value: "/" },
                                description: "Path to host your rust server on",
                                name: "api path",
                                typ: { tag: "nonEmptyStr" },
                              },
                              port: {
                                description:
                                  "Port to forward incoming http requests to",
                                example: "7000",
                                typ: { tag: "unsignedInt16" },
                              },
                            },
                            tag: "submodule",
                          },
                          tag: "nullOr",
                        },
                      },
                    },
                    tag: "submodule",
                  },
                  tag: "attrsOf",
                },
              },
            },
            tag: "submodule",
          },
        },
      },
    ],
  };
  fetchMock.doMock(async (req): Promise<MockResponseInit> => {
    const repoKeyUrlRegexp =
      /[/]api[/]keys[/]github[/]([^/]+)[/]([^/]+)[/]repo-key.public/;
    return match([req.method, req.url])
      .with(["GET", "/api/modules"], () => {
        if (savedConfig == null) {
          return { status: 404 };
        }
        return { status: 200, body: JSON.stringify(savedConfig) };
      })
      .with(["PUT", "/api/modules"], () => {
        savedConfig = JSON.parse(req.body!.toString());
        return { status: 200 };
      })
      .with(["GET", "/api/account/repos"], () => {
        const testRepos: { repos: Array<string> } = {
          repos: ["test-user/test-repo"],
        };
        return { status: 200, body: JSON.stringify(testRepos) };
      })
      .with(["GET", "/api/modules/available"], () => ({
        status: 200,
        body: JSON.stringify(availableModules),
      }))
      .with(["GET", "/mock.wasm"], () => ({
        status: 200,
        body: "mock-wasm",
      }))
      .with(["GET", P.string.regex(repoKeyUrlRegexp).select()], (url) => {
        const [_, repoUser, repoName] = url.match(repoKeyUrlRegexp)!;
        return {
          status: 200,
          body: `<REPO_KEY_FOR ${repoUser}/${repoName}>`,
        };
      })
      .otherwise(() => {
        console.error(`unmocked path: ${req.method} ${req.url}`);
        throw Error(`unmocked path: ${req.method} ${req.url}`);
      });
  });
};

beforeEach(() => {
  fetchMock.resetMocks();
  jest.resetAllMocks();
  searchParams = new URLSearchParams();
  savedConfig = null;
  setupApiMocks();
  user = userEvent.setup();
});

const selectTestRepo = async () => {
  const selectButton = await screen.findByText("Select");
  await user.click(selectButton);
};

const selectModule = async (moduleName: string) => {
  const checkbox = within(
    (await screen.findByText(`${moduleName} Module`)).parentElement!,
  ).getByRole("checkbox");
  await user.click(checkbox);
};

const findModuleField = async (name: string, field: string) =>
  within(
    (await screen.findByText(`Module Configuration for ${name}`))
      .parentElement!,
  ).getByText(field).parentElement!;

const expectNoErrors = () => {
  expect(screen.queryAllByTestId("error").map((el) => el.textContent)).toEqual(
    [],
  );
};

describe("modules config page", () => {
  it("renders dynamic module config forms", async () => {
    render(<Entry />);
    await selectTestRepo();
    await selectModule("PostgreSQL");
    await screen.findByText("Port");
    expectNoErrors();
  });

  const postgresModuleConfig = {
    repo_name: "test-repo",
    repo_user: "test-user",
    user_config: [
      {
        module_name: "PostgreSQL",
        git_commit: "c534ff50b006663651d6445e3c55e76da7fd9947",
        values: {
          tag: "set",
          value: {
            postgresql: {
              tag: "set",
              value: {
                "postgresql-project": {
                  tag: "set",
                  value: {
                    port: {
                      tag: "int",
                      value: 8080,
                    },
                  },
                },
              },
            },
          },
        },
      },
    ],
  };

  const postgresModuleConfigResponse = {
    ...postgresModuleConfig,
    modules: [
      {
        description: "A garnix module for postgreSQL",
        git_commit: "c534ff50b006663651d6445e3c55e76da7fd9947",
        name: "PostgreSQL",
        repo_name: "postgresql-module",
        repo_user: "garnix-io",
        schema: {
          description: "A garnix module for postgreSQL",
          typ: {
            fields: {
              postgresql: {
                description: "An attrset of postgresql databases",
                typ: {
                  fieldType: {
                    fields: {
                      port: {
                        default: { tag: "int", value: 5432 },
                        description: "The port in which to run PostgreSQL",
                        typ: { tag: "unsignedInt16" },
                      },
                    },
                    tag: "submodule",
                  },
                  tag: "attrsOf",
                },
              },
            },
            tag: "submodule",
          },
        },
      },
    ],
  };

  const simulateUserSave = async () => {
    await clickOpenPr();
    await user.click(screen.getByText("Cancel"));
  };

  const clickOpenPr = async () => {
    await act(async () => {
      class TestSubmitEvent extends Event {
        submitter = {
          getAttribute: (name: string) => {
            if (name === "data-submit-action") return "openPr";
            throw Error(`not a mocked event submitter attribute: ${name}`);
          },
        };
      }
      const event = new TestSubmitEvent("submit", { bubbles: true });
      const form = await screen.findByRole("form");
      form.dispatchEvent(event);
    });
  };

  const getSavedConfigTestModuleValue = () => {
    return (savedConfig as any).user_config[0].values.value.testModule.value[
      "testModule-project"
    ].value;
  };

  it("saves the module config", async () => {
    render(<Entry />);
    await selectTestRepo();
    await selectModule("PostgreSQL");
    const portField = within(
      await findModuleField("PostgreSQL", "Port"),
    ).getByRole("spinbutton");
    await user.clear(portField);
    await user.type(portField, "8080");
    await simulateUserSave();
    expect(savedConfig).toEqual(postgresModuleConfig);
    expectNoErrors();
  });

  it("fills in the values from a saved config", async () => {
    setupApiMocks();
    savedConfig = postgresModuleConfigResponse;
    render(<Entry />);
    const portField = within(
      await findModuleField("PostgreSQL", "Port"),
    ).getByRole("spinbutton");
    expect(portField).toHaveValue(8080);
    expectNoErrors();
  });

  describe("validation", () => {
    it("validates form inputs on clicking a button", async () => {
      render(<Entry />);
      await selectTestRepo();
      await selectModule("Rust");
      const srcField = within(
        await findModuleField("Rust", "Source Directory"),
      ).getByRole("textbox");
      await user.type(srcField, "foo bar");
      await clickOpenPr();
      expect(
        screen.queryAllByTestId("error").map((el) => el.textContent),
      ).toContain(
        "Errors in the module configuration for Rust: src: Path cannot contain illegal characters.",
      );
    });

    it("shows path validation next to the form field", async () => {
      render(<Entry />);
      await selectModule("Rust");
      const srcField = within(
        await findModuleField("Rust", "Source Directory"),
      ).getByRole("textbox");
      await user.type(srcField, "foo bar");
      const srcSection = await findModuleField("Rust", "Source Directory");
      expect(
        within(srcSection)
          .queryAllByTestId("error")
          .map((el) => el.textContent),
      ).toEqual(["Path cannot contain illegal characters."]);
    });

    it("shows unsignedInt16 validation next to the form field", async () => {
      render(<Entry />);
      await selectModule("Rust");
      const webserverToggle = within(
        await findModuleField("Rust", "Web Server"),
      ).getByRole("checkbox");
      await user.click(webserverToggle);
      const portField = within(await findModuleField("Rust", "Port")).getByRole(
        "spinbutton",
      );
      await user.click(portField);
      await user.clear(portField);
      await userEvent.keyboard("-1");
      const portSection = await findModuleField("Rust", "Port");
      expect(
        within(portSection)
          .queryAllByTestId("error")
          .map((el) => el.textContent),
      ).toEqual(["value must be positive"]);
    });
  });

  it("fills in defaults from module schemas", async () => {
    render(<Entry />);
    await selectModule("NodeJS");
    const testCommandField = within(
      await findModuleField("NodeJS", "Test Command"),
    ).getByRole("textbox");
    expect(testCommandField).toHaveValue("npm run test");
  });

  it("uses option names, if they exist", async () => {
    render(<Entry />);
    await selectModule("Rust");
    await screen.findByText("Source Directory");
  });

  it("show module selected using query parameters", async () => {
    searchParams = new URLSearchParams("selectedModules=NodeJS");
    render(<Entry />);
    const testCommandField = within(
      await findModuleField("NodeJS", "Test Command"),
    ).getByRole("textbox");
    expect(testCommandField).toHaveValue("npm run test");
  });

  describe("placeholders", () => {
    it("shows examples in text field placeholders", async () => {
      render(<Entry />);
      await selectModule("Rust");
      const webserverToggle = within(
        await findModuleField("Rust", "Web Server"),
      ).getByRole("checkbox");
      await user.click(webserverToggle);
      const commandField = within(
        await findModuleField("Rust", "Server Command"),
      ).getByRole("textbox");
      expect(commandField.getAttribute("placeholder")).toEqual(
        "e.g.: server --port 7000",
      );
    });

    it("falls back to the default, if that exists", async () => {
      render(<Entry />);
      await selectModule("Rust");
      const webserverToggle = within(
        await findModuleField("Rust", "Web Server"),
      ).getByRole("checkbox");
      await user.click(webserverToggle);
      const commandField = within(
        await findModuleField("Rust", "Api Path"),
      ).getByRole("textbox");
      expect(commandField.getAttribute("placeholder")).toEqual("e.g.: /");
    });
  });

  describe("paths", () => {
    const setupSrcModule = async (
      fieldSchema: Record<string, ModuleSchema>,
    ) => {
      setupTestModuleSchema(fieldSchema);
      render(<Entry />);
      await selectTestRepo();
      await selectModule("Test");
      const srcSection = await findModuleField("Test", "Src");
      const srcField = within(srcSection).getByRole("textbox");
      return { srcSection, srcField };
    };

    it("accepts paths without leading `./` and prepends it internally", async () => {
      const { srcSection, srcField } = await setupSrcModule({
        src: { typ: { tag: "path" } },
      });
      await user.clear(srcField);
      await user.type(srcField, "path");
      expect(
        within(srcSection)
          .queryAllByTestId("error")
          .map((el) => el.textContent),
      ).toEqual([]);
      await simulateUserSave();
      expect(srcField).toHaveValue("path");
      expect(getSavedConfigTestModuleValue().src.value).toEqual("./path");
    });

    const testCases: Array<[string, string]> = [
      [".", "./."],
      ["./.", "./."],
      ["./foo", "./foo"],
      ["./", "./."],
      ["/", "./."],
      ["/.", "./."],
      ["/foo", "./foo"],
      ["./foo/", "./foo"],
      ["foo/", "./foo"],
      ["/foo/", "./foo"],
      ["./foo/", "./foo"],
      ["././foo", "././foo"],
    ];
    for (const [input, normalized] of testCases) {
      it(`normalizes ${input} to ${normalized}`, () => {
        expect(_normalize(input)).toEqual(normalized);
      });
    }

    it("shows validation errors for empty paths", async () => {
      const { srcSection, srcField } = await setupSrcModule({
        src: { typ: { tag: "path" } },
      });
      await user.clear(srcField);
      expect(
        within(srcSection)
          .queryAllByTestId("error")
          .map((el) => el.textContent),
      ).toEqual(["Path cannot be empty."]);
    });

    it("shows initial values (defaults) for paths without leading `./`", async () => {
      const { srcField } = await setupSrcModule({
        src: { typ: { tag: "path" } },
      });
      expect(srcField).toHaveValue(".");
    });

    it("shows placeholder `./.` without leading `./`", async () => {
      const { srcField } = await setupSrcModule({
        src: { typ: { tag: "path" }, example: "./." },
      });
      expect(srcField.getAttribute("placeholder")).toEqual("e.g.: .");
    });

    it("shows placeholder `./path` without leading `./`", async () => {
      const { srcField } = await setupSrcModule({
        src: { typ: { tag: "path" }, example: "./path" },
      });
      expect(srcField.getAttribute("placeholder")).toEqual("e.g.: path");
    });

    it("shows placeholder `path` without leading `./`", async () => {
      const { srcField } = await setupSrcModule({
        src: { typ: { tag: "path" }, example: "path" },
      });
      expect(srcField.getAttribute("placeholder")).toEqual("e.g.: path");
    });
  });

  describe("multiple modules", () => {
    const postgresAndRustConfig = (
      args: { postgresPort?: number; rustSrc?: string } = {},
    ) => ({
      repo_user: "test-user",
      repo_name: "test-repo",
      user_config: [
        {
          module_name: "PostgreSQL",
          git_commit: "c534ff50b006663651d6445e3c55e76da7fd9947",
          values: {
            tag: "set",
            value: {
              postgresql: {
                tag: "set",
                value: {
                  "postgresql-project": {
                    tag: "set",
                    value: {
                      port: {
                        tag: "int",
                        value: args.postgresPort || 5432,
                      },
                    },
                  },
                },
              },
            },
          },
        },
        {
          module_name: "Rust",
          git_commit: "c534ff50b006663651d6445e3c55e76da7fd9947",
          values: {
            tag: "set",
            value: {
              rust: {
                tag: "set",
                value: {
                  "rust-project": {
                    tag: "set",
                    value: {
                      buildDependencies: {
                        tag: "list",
                        value: [],
                      },
                      devTools: {
                        tag: "list",
                        value: [],
                      },
                      runtimeDependencies: {
                        tag: "list",
                        value: [],
                      },
                      src: {
                        tag: "path",
                        value: args.rustSrc || "./.",
                      },
                      webServer: {
                        tag: "null",
                      },
                    },
                  },
                },
              },
            },
          },
        },
      ],
      modules: [
        {
          description: "A garnix module for postgreSQL",
          git_commit: "c534ff50b006663651d6445e3c55e76da7fd9947",
          name: "PostgreSQL",
          repo_name: "postgresql-module",
          repo_user: "garnix-io",
          schema: {
            description: "A garnix module for postgreSQL",
            typ: {
              fields: {
                postgresql: {
                  description: "An attrset of postgresql databases",
                  typ: {
                    fieldType: {
                      fields: {
                        port: {
                          default: { tag: "int", value: 5432 },
                          description: "The port in which to run PostgreSQL",
                          typ: { tag: "unsignedInt16" },
                        },
                      },
                      tag: "submodule",
                    },
                    tag: "attrsOf",
                  },
                },
              },
              tag: "submodule",
            },
          },
        },
        {
          description: null,
          git_commit: "c534ff50b006663651d6445e3c55e76da7fd9947",
          name: "Rust",
          repo_name: "rust-module",
          repo_user: "garnix-io",
          schema: {
            typ: {
              fields: {
                rust: {
                  description: "An attrset of rust projects to generate",
                  typ: {
                    fieldType: {
                      fields: {
                        buildDependencies: {
                          default: { tag: "list", value: [] },
                          description:
                            "A list of dependencies required to build this package. They are made available in the devshell, and at build time",
                          name: "build dependencies",
                          typ: {
                            elementType: { tag: "package" },
                            tag: "listOf",
                          },
                        },
                        devTools: {
                          default: { tag: "list", value: [] },
                          description:
                            "A list of packages make available in the devshell for this project (and `default` devshell). This is useful for things like LSPs, formatters, etc.",
                          name: "development tools",
                          typ: {
                            elementType: { tag: "package" },
                            tag: "listOf",
                          },
                        },
                        runtimeDependencies: {
                          default: { tag: "list", value: [] },
                          description:
                            "A list of dependencies required at runtime. They are made available in the devshell, at build time, and are available on the server at runtime",
                          name: "runtime dependencies",
                          typ: {
                            elementType: { tag: "package" },
                            tag: "listOf",
                          },
                        },
                        src: {
                          description:
                            "A path to the directory containing Cargo.lock, Cargo.toml, and src",
                          example: "./.",
                          name: "source directory",
                          typ: { tag: "path" },
                        },
                        webServer: {
                          default: { tag: "null" },
                          description:
                            "Whether to create an HTTP server based on this Rust project",
                          typ: {
                            innerType: {
                              fields: {
                                command: {
                                  description:
                                    "The command to run to start the server in production",
                                  example: "server --port 7000",
                                  name: "server command",
                                  typ: { tag: "nonEmptyStr" },
                                },
                                path: {
                                  default: { tag: "string", value: "/" },
                                  description:
                                    "Path to host your rust server on",
                                  name: "api path",
                                  typ: { tag: "nonEmptyStr" },
                                },
                                port: {
                                  description:
                                    "Port to forward incoming http requests to",
                                  example: "7000",
                                  typ: { tag: "unsignedInt16" },
                                },
                              },
                              tag: "submodule",
                            },
                            tag: "nullOr",
                          },
                        },
                      },
                      tag: "submodule",
                    },
                    tag: "attrsOf",
                  },
                },
              },
              tag: "submodule",
            },
          },
        },
      ],
    });

    it("allows selecting multiple modules", async () => {
      render(<Entry />);
      await selectTestRepo();
      await selectModule("PostgreSQL");
      await screen.findByText("Port");
      await selectModule("Rust");
      await screen.findByText("Build Dependencies");
      await simulateUserSave();
      const expectedRequestBody: any = postgresAndRustConfig();
      delete expectedRequestBody["modules"];
      expect(savedConfig).toEqual(expectedRequestBody);
    });

    it("allows unselecting a module", async () => {
      render(<Entry />);
      await selectTestRepo();
      await selectModule("Rust");
      await selectModule("PostgreSQL");
      await selectModule("Rust");
      expectNoErrors();
      await simulateUserSave();
      expectNoErrors();
      expect(savedConfig).toMatchInlineSnapshot(`
{
  "repo_name": "test-repo",
  "repo_user": "test-user",
  "user_config": [
    {
      "git_commit": "c534ff50b006663651d6445e3c55e76da7fd9947",
      "module_name": "PostgreSQL",
      "values": {
        "tag": "set",
        "value": {
          "postgresql": {
            "tag": "set",
            "value": {
              "postgresql-project": {
                "tag": "set",
                "value": {
                  "port": {
                    "tag": "int",
                    "value": 5432,
                  },
                },
              },
            },
          },
        },
      },
    },
  ],
}
`);
      expectNoErrors();
    });

    it("fills in multiple values from saved configs", async () => {
      savedConfig = postgresAndRustConfig({
        postgresPort: 8080,
        rustSrc: "./rust-src",
      });
      render(<Entry />);
      const postgresPortField = within(
        await findModuleField("PostgreSQL", "Port"),
      ).getByRole("spinbutton");
      expect(postgresPortField).toHaveValue(8080);
      const rustSrcField = within(
        await findModuleField("Rust", "Source Directory"),
      ).getByRole("textbox");
      expect(rustSrcField).toHaveValue("rust-src");
    });
  });

  describe("markdown", () => {
    const getTestSrcDescription = async (): Promise<string | null> => {
      await selectModule("Test");
      const srcField = await findModuleField("Test", "Src");
      const text = within(srcField).getByTestId("description").innerHTML;
      if (text == null) throw new Error("cannot find field description");
      return text.trim();
    };

    it("converts option descriptions from markdown into html", async () => {
      setupTestModuleSchema({
        src: {
          typ: { tag: "path" },
          description: endent`
            # Heading

            This is some paragraph
            in multiple lines.

            Another paragraph,
            also in multiple lines. With dots.
          `,
        },
      });
      render(<Entry />);
      expect(await getTestSrcDescription()).toEqual(
        endent`
          <h1>Heading</h1>
          <p>This is some paragraph
          in multiple lines.</p>
          <p>Another paragraph,
          also in multiple lines. With dots.</p>
        `,
      );
    });

    it("converts module descriptions", async () => {
      setupTestModuleSchema(
        {
          src: {
            typ: { tag: "path" },
          },
        },
        { description: "[markdown link](example.com)" },
      );
      render(<Entry />);
      const description = within(
        (await screen.findByText("Test Module")).parentElement!,
      )
        .getByTestId("description")
        .innerHTML.trim();
      expect(description).toEqual(
        '<p><a href="example.com">markdown link</a></p>',
      );
    });

    it("supports links in markdown format", async () => {
      setupTestModuleSchema({
        src: {
          typ: { tag: "path" },
          description: "[link text](http://example.com)",
        },
      });
      render(<Entry />);
      expect(await getTestSrcDescription()).toEqual(
        '<p><a href="http://example.com">link text</a></p>',
      );
    });

    it("sanitizes html contents in markdown", async () => {
      setupTestModuleSchema({
        src: {
          typ: { tag: "path" },
          description:
            '<button onclick="() => doSomethingEvil()">click me!</button>',
        },
      });
      render(<Entry />);
      expect(await getTestSrcDescription()).toEqual(
        "<p><button>click me!</button></p>",
      );
    });
  });

  describe("secrets", () => {
    it("encrypts secrets for the selected repo", async () => {
      setupTestModuleSchema({
        password: {
          typ: { tag: "encryptedSecret" },
        },
      });
      render(<Entry />);
      await selectTestRepo();
      await selectModule("Test");
      const passwordSelection = await findModuleField("Test", "Password");
      const openSetSecretModal = within(passwordSelection).getByRole("button");
      await user.click(openSetSecretModal);
      const form = screen.getByTestId("set-secret-form");
      const secretField = within(form).getByRole("textbox");
      await user.type(secretField, "hunter2");
      await user.click(within(form).getByText("Save"));
      await simulateUserSave();
      expect(getSavedConfigTestModuleValue().password.value).toEqual({
        encryptedFor: {
          repoUser: "test-user",
          repoName: "test-repo",
        },
        encryptedValue:
          "<MOCK_ENCRYPTED_FOR <REPO_KEY_FOR test-user/test-repo> hunter2>",
      });
      expectNoErrors();
    });
  });
});
