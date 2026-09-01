"use client";

import {
  createContext,
  useContext,
  PropsWithChildren,
  useState,
  useCallback,
  useEffect,
} from "react";
import { useRouter } from "next/navigation";
import { match, P } from "ts-pattern";
import {
  getCurrentUser,
  getSignupLink,
  logout as logoutService,
} from "@/services/auth";
import { Err, Ok, onUnauthorized } from "@/services";

export type User = {
  name: string;
  email?: string;
};

type UserState =
  | { state: "loading" }
  | { state: "logged-in"; user: User }
  | { state: "logged-out" };

type UserContextType = {
  user: UserState;
  signupLink: string | undefined;
  setUser: (user: User) => void;
  logout: () => Promise<void>;
};

const defaultValue: UserContextType = {
  user: { state: "loading" },
  signupLink: undefined,
  setUser: () => {},
  logout: async () => {},
};

const UserContext = createContext<UserContextType>(defaultValue);

export const UserProvider = ({ children }: PropsWithChildren) => {
  const [user, setUserState] = useState<UserState>({ state: "loading" });
  const [signupLink, setSignupLink] = useState<string>();
  const router = useRouter();
  const setUser = useCallback((user: User) => {
    setUserState({ state: "logged-in", user });
  }, []);
  const logout = useCallback(async () => {
    await logoutService();
    setUserState({ state: "logged-out" });
    router.replace("/");
  }, [router]);
  useEffect(() => {
    onUnauthorized(() => {
      void logoutService().finally(() => setUserState({ state: "logged-out" }));
    });
    return () => onUnauthorized(null);
  }, []);
  useEffect(() => {
    void (async () => {
      const curUser = await getCurrentUser();
      setUserState(
        match(curUser)
          .with(Ok(null), () => ({ state: "logged-out" as const }))
          .with(Ok(P.select(P.not(null))), (user) => ({
            state: "logged-in" as const,
            user,
          }))
          .with(Err(P.select()), (err) => {
            console.error("Failed to read current user:", err.message);
            return { state: "logged-out" as const };
          })
          .exhaustive(),
      );
      const result = await getSignupLink();
      if (result.ok) setSignupLink(result.data);
    })();
  }, []);
  return (
    <UserContext.Provider value={{ user, signupLink, setUser, logout }}>
      {children}
    </UserContext.Provider>
  );
};

export const useUser = (): UserContextType => {
  return useContext(UserContext);
};
