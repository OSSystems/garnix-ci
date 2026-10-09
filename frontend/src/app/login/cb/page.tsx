"use client";

import { Suspense } from "react";
import { LoginCallback } from "@/components/loginCallback";
import { defaultForgeSlug } from "@/services/forges";

// github.com's OAuth app calls back here, as it always has.
const Page = () => (
  <Suspense fallback={null}>
    <LoginCallback forge={defaultForgeSlug} />
  </Suspense>
);

export default Page;
