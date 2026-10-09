"use client";

import { Suspense } from "react";
import { LoginCallback } from "@/components/loginCallback";

// Every forge but github.com calls back here, under its slug.
const Page = ({ params }: { params: { forge: string } }) => (
  <Suspense fallback={null}>
    <LoginCallback forge={decodeURIComponent(params.forge)} />
  </Suspense>
);

export default Page;
