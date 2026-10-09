"use client";

import { Register, ToLogin } from "./register";

type PageProps = {
  searchParams: Record<string, string>;
};

// Only the login page's answer opens this page, with the URL it was given.
const Page = (props: PageProps) => {
  const url = props.searchParams.url ?? "";
  return url === "" ? <ToLogin /> : <Register url={url} />;
};

export default Page;
