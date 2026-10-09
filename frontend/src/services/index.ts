import { ZodTypeDef, z } from "zod";
import { formatZodError } from "@/utils/zod";

export type Ok<T> = { ok: true; data: T };
export type Err<E> = { ok: false; error: E };
export type Result<T, E = { message: string }> = Ok<T> | Err<E>;

export const Ok = <T>(data: T): Ok<T> => ({ ok: true, data });
export const Err = <E>(error: E): Err<E> => ({ ok: false, error });

export type APIError = {
  path: string;
  reason: "not-ok" | "server-error" | "schema-invalid";
  message: string;
  status: number | "parse-error";
  // The machine-readable reason a refusal carries, when it carries one.
  refusal?: string;
};

export type APIResult<T> = Result<T, APIError>;

let unauthorizedListener: (() => void) | null = null;

export const onUnauthorized = (listener: (() => void) | null): void => {
  unauthorizedListener = listener;
};

export const fetchFromAPI = async <Input, Output>(
  schema: z.Schema<Output, ZodTypeDef, Input>,
  method: "GET" | "POST" | "DELETE" | "PUT",
  path: string,
  options?: {
    query?: URLSearchParams | Record<string, string>;
    body?: BodyInit;
    apiOrigin?: string;
  },
): Promise<APIResult<Output>> => {
  let url = `${options?.apiOrigin || ""}/api/${path}`;
  if (method === "GET" && options?.query)
    url += `?${new URLSearchParams(options.query).toString()}`;
  const finalOptions: RequestInit = {
    ...(options || {}),
    method,
  };
  // A DELETE may carry a body too (disconnecting a forge identity does).
  if (method !== "GET" && finalOptions?.body)
    finalOptions.headers = {
      ...finalOptions.headers,
      "Content-Type": "application/json",
    };
  const response = await fetch(url, finalOptions);
  const rawBody = await response.text();
  const body = safeParseJson(rawBody);
  if (!response.ok) {
    if (response.status === 401) unauthorizedListener?.();
    return Err({
      path,
      reason: "not-ok",
      message: body.message || rawBody || response.statusText || "error",
      status: response.status,
      ...(typeof body.reason === "string" ? { refusal: body.reason } : {}),
    });
  }
  const verifiedResponse = schema.safeParse(body);
  if (!verifiedResponse.success)
    return Err({
      path,
      reason: "schema-invalid",
      message: formatZodError(verifiedResponse.error),
      status: "parse-error",
    });
  return Ok(verifiedResponse.data);
};

const safeParseJson = (text: string): any => {
  try {
    return JSON.parse(text);
  } catch {
    return {};
  }
};

// The backend's message, without the status it prefixes some with: it is
// written for the person who sees it.
export const userMessage = (error: APIError): string =>
  error.message.replace(/^(Bad Request|Forbidden|Unauthorized): /, "");
