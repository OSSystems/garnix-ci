"use client";

import { Register, ToLogin } from "./register";

type PageProps = {
  searchParams: Record<string, string>;
};

// Only the login or account page's answer opens this page, with the URL it was given.
const Page = (props: PageProps) => {
  const url = props.searchParams.url ?? "";
  const from = props.searchParams.from === "account" ? "account" : "login";
  return url === "" ? (
    <ToLogin from={from} />
  ) : (
    <Register url={url} from={from} />
  );
};

export default Page;
