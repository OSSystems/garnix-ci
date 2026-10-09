"use client";

import { P, match } from "ts-pattern";
import { ReactNode } from "react";
import React from "react";
import { redirect, useSearchParams } from "next/navigation";
import endent from "endent";
import { Text } from "@/components/text";
import { Berlin } from "@/utils/fonts";
import { Modal, ModalActions, ModalSection } from "@/components/modal";
import { WithSidebar } from "@/components/withSidebar";
import { useField, useForm } from "@/hooks/useForm";
import { useLoading } from "@/hooks/useLoading";
import {
  Module,
  ModuleFormConfig,
  ModuleSchema,
  ModuleSchemaType,
  Repo,
  getAvailableModules,
  getModuleConfig,
  initialValueForSchema,
  peelOffToplevel,
  toModuleRecord,
  uploadModuleConfigs,
  validate,
} from "@/services/modules";
import { Button } from "@/components/button";
import { InputProps, IntInput, Select, TextInput } from "@/components/input";
import { Err, Ok, Result } from "@/services";
import { ToggleSwitch } from "@/components/toggleSwitch";
import { filterNull, mapCollectResult, mapValues } from "@/utils";
import { capitalizeWords, nixFieldToHumanReadable } from "@/utils/format";
import { Expander } from "@/components/expander";
import { NixValue } from "@/services/modules/nixValue";
import { useDebounced } from "@/hooks/useDebounced";
import { useUser } from "@/store/userContext";
import { useLoginLinkForCurrentPage } from "@/hooks/useLoginLinkForCurrentPage";
import { useConfig } from "@/store/configContext";
import { ListOfPackages } from "./moduleInputs/listOfPackages";
import styles from "./styles.module.css";
import { RepoPicker } from "./moduleInputs/repoPicker";
import { ModuleSelector } from "./moduleInputs/moduleSelector";
import { PathInput } from "./moduleInputs/nixPath";
import { OpenPrModal } from "./openPrModal";
import { ResetModal } from "./resetModal";
import { PreviewModal } from "./previewModal";
import { Markdown } from "./markdown";
import { ListInput } from "./moduleInputs/list";
import { SecretInput } from "./moduleInputs/secretInput";

const Page = () => {
  const state = useUser().user.state;
  const loginLink = useLoginLinkForCurrentPage().loginLink;
  return match(state)
    .with("loading", () => <WithSidebar />)
    .with("logged-out", () => redirect(loginLink))
    .with("logged-in", () => (
      <WithSidebar>
        <div className={styles.container}>
          <Modal>
            <Entry />
          </Modal>
        </div>
      </WithSidebar>
    ))
    .exhaustive();
};

export const Entry = () => {
  const savedValues = useLoading(getModuleConfig);
  if (savedValues.loading) {
    return null;
  }
  if (!savedValues.data.ok) {
    return <Error {...savedValues.data.error} />;
  }
  const savedRaw = savedValues.data.data;
  const saved = savedRaw && {
    repo: {
      repoUser: savedRaw.repo_user,
      repoName: savedRaw.repo_name,
    },
    user_config: Object.fromEntries(
      savedRaw.user_config.map((module) => [module.module_name, module.values]),
    ),
    modules: savedRaw.modules,
  };
  return <RepoAndModuleValuesForm saved={saved} />;
};

const FieldWrapper = (props: {
  label: string;
  description: string;
  markdown?: boolean;
  children: React.ReactNode;
}) => (
  <fieldset className={styles.fieldSet}>
    <legend className={styles.fieldWrapperLabel}>{props.label}</legend>
    {props.markdown ? (
      <Markdown markdown={props.description} />
    ) : (
      <p className={`${Berlin.className} ${styles.description}`}>
        {props.description}
      </p>
    )}
    {props.children}
  </fieldset>
);

const getModulesIfNeeded = async (
  props: null | Array<Module>,
): Promise<Result<Record<string, Module>>> => {
  if (props == null) {
    return await getAvailableModules();
  }
  return Ok(toModuleRecord(props));
};

const RepoAndModuleValuesForm = (props: {
  saved: null | {
    repo: Repo;
    user_config: Record<string, NixValue>;
    modules: Array<Module>;
  };
}) => {
  const search = useSearchParams();
  const repo = useField<{ repoUser: string; repoName: string } | null>(
    props.saved == null ? null : props.saved.repo,
  );
  const moduleNames = useField<Array<string>>(
    props.saved
      ? Object.keys(props.saved.user_config)
      : search.getAll("selectedModules"),
  );
  const { githubAppName } = useConfig();

  const modules = props.saved == null ? null : props.saved.modules;
  const loadingAvailableModules = useLoading(
    React.useCallback(() => getModulesIfNeeded(modules), [modules]),
  );
  if (loadingAvailableModules.loading) {
    return null;
  }
  if (!loadingAvailableModules.data.ok)
    return <Error {...loadingAvailableModules.data.error} />;
  const availableModules = loadingAvailableModules.data.data;
  const formConfigResult = mapCollectResult(
    (moduleName: string): Result<[string, ModuleFormConfig]> => {
      const mod = availableModules[moduleName];
      if (mod == null)
        return Err({ message: `Cannot find module: ${moduleName}` });
      const formConfig = peelOffToplevel(
        mod.schema,
        props.saved?.user_config[moduleName] || null,
        mod.git_commit,
      );
      if (!formConfig.ok) return formConfig;
      return Ok([moduleName, formConfig.data]);
    },
    moduleNames.value,
  );
  if (!formConfigResult.ok) return <Error {...formConfigResult.error} />;
  const formConfigs = Object.fromEntries(formConfigResult.data);
  return (
    <>
      <ModalSection>
        <Text type="h1" className={styles.h1}>
          garnix modules
        </Text>
      </ModalSection>
      <ModalSection>
        <Text className={styles.textWithPadding}>
          garnix modules make it easy to set up CI and server hosting for your
          code.
        </Text>
        <FieldWrapper
          label="Source Repo"
          description={endent`
            Choose the repository you want to set up CI (and optionally hosting) for.

            If you can't find the repository you're looking for below, make sure to
            [enable the garnix GitHub app](https://github.com/apps/${githubAppName})
            for it.

            If you want to start with an empty repo [create it here](https://github.com/new).
          `}
          markdown
        >
          <RepoPicker {...repo.props} />
        </FieldWrapper>
        <FieldWrapper
          label="Module Selection"
          description={endent`
            Choose the garnix modules that you want to use. Once you pick one, more configuration options will show up below.

            You can pick multiple modules to create more complex configuration, for example for repos that contain code in multiple languages.
          `}
          markdown
        >
          <ModuleSelector {...moduleNames.props} modules={availableModules} />
        </FieldWrapper>
      </ModalSection>
      {
        <ModuleSchemaForm
          repoSelection={repo.value}
          formConfigs={formConfigs}
        />
      }
    </>
  );
};

const ModuleSchemaForm = (props: {
  repoSelection: Repo | null;
  formConfigs: Record<string, ModuleFormConfig>;
}) => {
  const isRepoSelected = props.repoSelection != null;
  const [showResetModal, setShowResetModal] = React.useState(false);
  const [showOpenPrModal, setShowOpenPrModal] = React.useState(false);
  const [showPreviewModal, setShowPreviewModal] = React.useState(false);
  const save = async () => {
    return await uploadModuleConfigs(
      props.repoSelection,
      props.formConfigs,
      moduleNixValues.value,
    );
  };
  const moduleNixValues = useField<Record<string, NixValue>>(
    mapValues((formConfig) => formConfig.initialValue, props.formConfigs),
  );
  const debouncedSave = useDebounced(() => void save(), 1000);
  debouncedSave.enqueue();
  const form = useForm({ moduleNixValues }, async (_, submitAction) => {
    debouncedSave.clear();
    const result = await save();
    if (!result.ok) return result;
    return await match(submitAction)
      .with("reset", async () => {
        setShowResetModal(true);
        return Ok(null);
      })
      .with("preview", async () => {
        setShowPreviewModal(true);
        return Ok(null);
      })
      .with("openPr", async () => {
        setShowOpenPrModal(true);
        return Ok(null);
      })
      .otherwise(() =>
        Err({
          message: `Unknown submit action ${JSON.stringify(submitAction)}`,
        }),
      );
  });
  return (
    <>
      <form role="form" {...form.props}>
        {Object.entries(props.formConfigs).map(([moduleName, formConfig]) => {
          let moduleNixValue = moduleNixValues.value[moduleName];
          if (moduleNixValue == null) {
            moduleNixValue = formConfig.initialValue;
            moduleNixValues.props.onChange({
              ...moduleNixValues.value,
              [moduleName]: moduleNixValue,
            });
          }
          return (
            <ModalSection
              key={moduleName}
              className={form.loading ? styles.loading : ""}
            >
              <Text type="h2">Module Configuration for {moduleName}</Text>
              <ModuleSchemaInput
                repoSelection={props.repoSelection}
                label={`${formConfig.stackName}.${formConfig.projectName}`}
                schema={formConfig.moduleSchema}
                value={moduleNixValue}
                onChange={(value) => {
                  moduleNixValues.props.onChange({
                    ...moduleNixValues.value,
                    [moduleName]: value,
                  });
                }}
              />
            </ModalSection>
          );
        })}
        <ModalSection>
          <ModalActions align="right">
            {form.result && !form.result.ok ? (
              <div style={{ flexGrow: 1 }}>
                <Error {...form.result.error} />
              </div>
            ) : null}
            <Button
              submit
              submitAction="reset"
              loading={form.loading || !isRepoSelected}
            >
              Reset
            </Button>
            <Button
              submit
              submitAction="preview"
              loading={form.loading || !isRepoSelected}
            >
              Preview
            </Button>
            <Button
              style="primaryInverse"
              submit
              submitAction="openPr"
              loading={form.loading || !isRepoSelected}
            >
              Create a Pull Request
            </Button>
          </ModalActions>
        </ModalSection>
      </form>
      {showResetModal && (
        <ResetModal onRequestClose={() => setShowResetModal(false)} />
      )}
      {showOpenPrModal && (
        <OpenPrModal onRequestClose={() => setShowOpenPrModal(false)} />
      )}
      {showPreviewModal && (
        <PreviewModal onRequestClose={() => setShowPreviewModal(false)} />
      )}
    </>
  );
};

const FieldValidation = (props: {
  schema: ModuleSchemaType;
  value: NixValue;
}) => {
  const result = validate({
    moduleSchema: props.schema,
    moduleValue: props.value,
  });
  if (!result.ok) return <Error {...result.error} />;
  return null;
};

const ModuleSchemaInput = (
  props: {
    repoSelection: Repo | null;
    label: string;
    schema: ModuleSchemaType;
    placeholder?: string;
  } & InputProps<NixValue>,
): ReactNode => {
  return (
    match(props.schema)
      // Special case handlers
      .with({ tag: "listOf", elementType: { tag: "package" } }, () => (
        <>
          <ListOfPackages
            value={filterNull(
              props.value.tag === "list"
                ? props.value.value.map((pkgNixExpr) =>
                    pkgNixExpr.tag === "raw" ? pkgNixExpr.value : null,
                  )
                : [],
            )}
            onChange={(value) =>
              props.onChange({
                tag: "list",
                value: value.map((v) => ({ tag: "raw", value: v })),
              })
            }
          />
          <FieldValidation {...props} />
        </>
      ))
      // Fallbacks
      .with({ tag: P.union("str", "nonEmptyStr", "package") }, () => {
        if (!(props.value.tag === "string" || props.value.tag === "raw")) {
          return <Error message='expected "str", "nonEmptyStr" or "package"' />;
        }
        const tag = props.value.tag;
        return (
          <>
            <TextInput
              label={props.label}
              placeholder={addPlaceholderEg(props.placeholder)}
              value={props.value.value}
              onChange={(value) => props.onChange({ tag, value })}
            />
            <FieldValidation {...props} />
          </>
        );
      })
      .with({ tag: "encryptedSecret" }, () => {
        if (props.value.tag !== "encryptedSecret") {
          return <Error message='expected "encryptedSecret"' />;
        }
        const tag = props.value.tag;
        return (
          <>
            <SecretInput
              label={props.label}
              placeholder={addPlaceholderEg(props.placeholder)}
              value={props.value.value}
              onChange={(value) => props.onChange({ tag, value })}
              repo={props.repoSelection}
            />
            <FieldValidation {...props} />
          </>
        );
      })
      .with({ tag: "path" }, () => {
        if (props.value.tag !== "path") {
          return <Error message='expected "path"' />;
        }
        const tag = props.value.tag;
        return (
          <>
            <PathInput
              label={props.label}
              placeholder={props.placeholder}
              value={props.value.value}
              onChange={(value) => props.onChange({ tag, value })}
            />
            <FieldValidation {...props} />
          </>
        );
      })
      .with({ tag: "bool" }, () => {
        if (props.value.tag !== "bool") {
          return <Error message='expected "bool"' />;
        }
        return (
          <>
            <ToggleSwitch
              className={styles.toggle}
              value={props.value.value}
              onChange={(value) => props.onChange({ tag: "bool", value })}
            />
            <FieldValidation {...props} />
          </>
        );
      })
      .with({ tag: "unsignedInt16" }, () => {
        if (props.value.tag !== "int") {
          return <Error message='expected "int"' />;
        }
        return (
          <>
            <IntInput
              label={props.label}
              value={props.value.value}
              onChange={(value: number) =>
                props.onChange({ tag: "int", value })
              }
            />
            <FieldValidation {...props} />
          </>
        );
      })
      .with({ tag: "int" }, () => {
        if (props.value.tag !== "int") {
          return <Error message='expected "int"' />;
        }
        return (
          <>
            <IntInput
              label={props.label}
              value={props.value.value}
              onChange={(value: number) =>
                props.onChange({ tag: "int", value })
              }
            />
            <FieldValidation {...props} />
          </>
        );
      })
      .with({ tag: "enum" }, (schema) => {
        if (props.value.tag !== "string") {
          return <Error message='expected "string"' />;
        }
        return (
          <>
            <Select
              label={props.label}
              value={props.value.value}
              onChange={(value: string) =>
                props.onChange({ tag: "string", value })
              }
            >
              <option value="">Select</option>
              {schema.variants.map((variant) => (
                <option key={variant} value={variant}>
                  {variant}
                </option>
              ))}
            </Select>
            <FieldValidation {...props} />
          </>
        );
      })
      .with({ tag: "submodule" }, (schema) => {
        return Object.entries(schema.fields).map(([field, fieldSchema]) => {
          if (props.value.tag !== "set") {
            return <Error key={field} message='expected "submodule"' />;
          }
          const value = props.value.value[field];
          if (value === undefined) {
            return (
              <Error
                key={field}
                message={`expected field value for "${field}"`}
              />
            );
          }
          const setValue = props.value;
          return (
            <FieldWrapper
              key={field}
              label={
                fieldSchema.name
                  ? capitalizeWords(fieldSchema.name)
                  : nixFieldToHumanReadable(field)
              }
              description={fieldSchema.description ?? ""}
              markdown
            >
              <ModuleSchemaInput
                repoSelection={props.repoSelection}
                label={""}
                schema={fieldSchema.typ}
                placeholder={moduleSchemaToPlaceholder(fieldSchema)}
                value={value}
                onChange={(v) => {
                  props.value;
                  props.onChange({
                    tag: "set",
                    value: { ...setValue.value, [field]: v },
                  });
                }}
              />
            </FieldWrapper>
          );
        });
      })
      .with(
        { tag: "listOf", elementType: P.select() },
        (elementType: ModuleSchemaType) => {
          if (props.value.tag !== "list") {
            return <Error message='expected "listOf"' />;
          }
          return (
            <ListInput
              label={props.label}
              initialElementValue={initialValueForSchema(elementType)}
              renderChild={({ onChange, value }) => (
                <ModuleSchemaInput
                  repoSelection={props.repoSelection}
                  label={""}
                  schema={elementType}
                  value={value}
                  onChange={onChange}
                />
              )}
              value={props.value.value}
              onChange={(value) => props.onChange({ tag: "list", value })}
            />
          );
        },
      )
      .with({ tag: "attrsOf" }, () => {
        if (props.value.tag !== "set") {
          return <Error message='expected "attrsOf"' />;
        }
        return (
          <Error message='Nested "attrsOf" options are not supported yet.' />
        );
      })
      .with(
        { tag: "nullOr", innerType: P.select() },
        (innerType: ModuleSchemaType) => {
          return (
            <NullOrInput
              label={props.label}
              initialElementValue={initialValueForSchema(innerType)}
              value={props.value.tag === "null" ? null : props.value}
              onChange={(newValue) => {
                if (newValue === null) {
                  props.onChange({ tag: "null" });
                } else {
                  props.onChange(newValue);
                }
              }}
              renderChild={({ value, onChange }) => (
                <ModuleSchemaInput
                  repoSelection={props.repoSelection}
                  label={props.label}
                  schema={innerType}
                  value={value}
                  onChange={onChange}
                />
              )}
            />
          );
        },
      )
      .exhaustive()
  );
};

const moduleSchemaToPlaceholder = (
  schema: ModuleSchema,
): string | undefined => {
  let result = undefined;
  if (schema.example) result = schema.example;
  if (schema.default)
    result = match(schema.default)
      .with(
        { tag: P.union("string", "path", "raw"), value: P.select() },
        (s: string) => s,
      )
      .with({ tag: "int", value: P.select() }, (int: number) => int.toString())
      .otherwise(() => undefined);
  return result;
};

export const addPlaceholderEg = (s: string | undefined): string | undefined =>
  s === undefined ? undefined : `e.g.: ${s}`;

const NullOrInput = <T,>(
  props: InputProps<T | null> & {
    label: string;
    initialElementValue: T;
    renderChild: (props: InputProps<T>) => React.ReactNode;
  },
) => {
  const lastNonNullValue = React.useRef(
    props.value ?? props.initialElementValue,
  );
  React.useEffect(() => {
    if (props.value != null) lastNonNullValue.current = props.value;
  }, [props.value]);
  return (
    <>
      <ToggleSwitch
        className={styles.toggle}
        value={props.value !== null}
        onChange={() => {
          if (props.value === null) {
            props.onChange(lastNonNullValue.current);
          } else {
            props.onChange(null);
          }
        }}
      />
      <Expander isOpen={props.value !== null}>
        {props.renderChild(
          props.value
            ? { value: props.value, onChange: props.onChange }
            : { value: lastNonNullValue.current, onChange: () => {} },
        )}
      </Expander>
    </>
  );
};

export const Error = (props: { message: string }) => (
  <Text className={styles.error} data-testid="error">
    {props.message}
  </Text>
);

export default Page;
