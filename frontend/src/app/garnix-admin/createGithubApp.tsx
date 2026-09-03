import { useSearchParams } from "next/navigation";
import { useEffect, useState } from "react";
import { Err, Ok, Result } from "@/services";

const newManifest = () => {
  const origin = getOrigin();
  return {
    url: origin,
    hook_attributes: {
      url: `${origin}/api/events/github/`,
    },
    redirect_url: `${origin}/garnix-admin`,
    callback_urls: [`${origin}/signup/fill`, `${origin}/login/cb`],
    description: "Garnix CI app",
    public: true,
    default_permissions: {
      checks: "write",
      contents: "read",
      metadata: "read",
      // write, not read: commenting on a PR when checks fail (garnix.yaml
      // `commentOnFailure`) is the only way to produce a Github inbox
      // notification. Existing installations must accept this permission.
      pull_requests: "write",
      statuses: "write",
      emails: "read",
      members: "read",
    },
    default_events: ["check_suite", "fork", "pull_request", "check_run"],
  };
};

const handleAppCreation = async (
  code: string,
): Promise<Result<{ [key: string]: string }>> => {
  const response = await fetch(
    `https://api.github.com/app-manifests/${code}/conversions`,
    { method: "POST" },
  );
  if (response.status < 200 || response.status >= 300) {
    return Err({
      message: `${response.status} ${response.statusText}: ${await response.text()}`,
    });
  }
  const json = await response.json();
  return Ok({
    GITHUB_APP_ID: json.id,
    GITHUB_CLIENT_ID: json.client_id,
    GITHUB_APP_NAME: json.slug,
    GITHUB_APP_PK: json.pem,
    GITHUB_WEBHOOK_SECRET: json.webhook_secret,
    GITHUB_CLIENT_SECRET: json.client_secret,
    GARNIX_URL: getOrigin(),
  });
};

const getOrigin = () => window.location.origin;

const CreateGithubApp = ({ setError }: { setError: (msg: string) => void }) => {
  const params = useSearchParams();
  const [githubAppValues, setGithubAppValues] = useState<{
    [key: string]: string;
  } | null>(null);

  useEffect(() => {
    void (async () => {
      const code = params.get("code");
      if (code != null) {
        const values = await handleAppCreation(code);
        if (!values.ok) {
          setError(values.error.message);
        } else {
          setGithubAppValues(values.data);
        }
      }
    })();
  }, [params, setError]);

  if (githubAppValues != null) {
    return (
      <>
        Here are the credentials for your GitHub app (See the README.md for how
        to use them):
        <br />
        <ShellSourceable values={githubAppValues} />
      </>
    );
  }

  const manifest = newManifest();
  return (
    <form action="https://github.com/settings/apps/new" method="post">
      <h2>Create New GitHub App</h2>
      <br />
      You can tweak the app manifest here:
      <br />
      <textarea
        name="manifest"
        defaultValue={JSON.stringify(manifest, null, 2)}
        style={{ width: "60%", height: "10em" }}
      />
      <br />
      <input type="submit" value="Submit to GitHub" />
    </form>
  );
};

const ShellSourceable = ({ values }: { values: { [key: string]: string } }) => {
  let text = "";
  for (const [key, value] of Object.entries(values)) {
    text += `export ${key}="${value}"\n`;
  }
  return (
    <pre>
      <textarea
        contentEditable={false}
        style={{ width: "60%", height: "20em" }}
      >
        {text}
      </textarea>
    </pre>
  );
};

export default CreateGithubApp;
