"use client";

import {
  createContext,
  useContext,
  PropsWithChildren,
  useState,
  useEffect,
} from "react";
import { getConfig } from "@/services/config";
import { Forge, findForge, getForges, githubForge } from "@/services/forges";

type ConfigContextType = {
  githubAppName: string;
  forges: Array<Forge>;
};

const defaultValue = {
  githubAppName: "",
  forges: [githubForge],
};

export const ConfigContext = createContext<ConfigContextType>(defaultValue);

export const ConfigProvider = ({ children }: PropsWithChildren) => {
  const [githubAppName, setGithubAppName] = useState("");
  const [forges, setForges] = useState(defaultValue.forges);
  useEffect(() => {
    void (async () => {
      const config = await getConfig();
      if (!config.ok) return;
      setGithubAppName(config.data.githubAppName);
    })();
    void (async () => {
      const result = await getForges();
      if (!result.ok) return;
      setForges(result.data);
    })();
  }, [setGithubAppName, setForges]);
  return (
    <ConfigContext.Provider value={{ githubAppName, forges }}>
      {children}
    </ConfigContext.Provider>
  );
};

export const useConfig = () => {
  return useContext(ConfigContext);
};

// The forge instance a repository lives on, once /api/forges has named it.
export const useForge = (slug: string): Forge | undefined =>
  findForge(useConfig().forges, slug);
