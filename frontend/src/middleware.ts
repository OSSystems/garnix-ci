import { NextRequest, NextResponse } from "next/server";

export function middleware(request: NextRequest) {
  const nonce = Buffer.from(crypto.randomUUID()).toString("base64");
  const contentSecurityPolicy = [
    `default-src 'none'`,
    `connect-src 'self' https://maps.googleapis.com https://plausible.io  https://api.github.com`,
    `script-src ${[
      "'self'",
      `'nonce-${nonce}'`,
      ...(process.env.NODE_ENV !== "production" ? ["'unsafe-eval'"] : []),
      "https://maps.googleapis.com",
    ].join(" ")}`,
    `frame-src https://www.loom.com`,
    `style-src 'self' 'unsafe-inline'`,
    `img-src 'self' https: blob: data:`,
    `font-src 'self'`,
    `media-src 'self'`,
    `object-src 'none'`,
    `base-uri 'none'`,
    `form-action 'self' https://github.com`,
    `frame-ancestors 'none'`,
  ].join(";");
  const requestHeaders = new Headers(request.headers);
  requestHeaders.set("x-nonce", nonce);
  const response = NextResponse.next({
    request: {
      headers: requestHeaders,
    },
  });

  response.headers.set("Content-Security-Policy", contentSecurityPolicy);
  response.headers.set("Referrer-Policy", "strict-origin-when-cross-origin");
  response.headers.set("X-Content-Type-Options", "nosniff");
  response.headers.set("X-Frame-Options", "DENY");

  return response;
}
