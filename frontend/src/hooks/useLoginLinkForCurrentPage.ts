import { usePathname, useSearchParams } from "next/navigation";

export function useLoginLinkForCurrentPage(): {
  loginLink: string;
} {
  let pathName = usePathname();
  const search = useSearchParams();
  if (search.size > 0) pathName += `?${search.toString()}`;
  const queryParams = `?page=${encodeURIComponent(search.get("page") ?? pathName)}`;
  return {
    loginLink: `/login${queryParams}`,
  };
}
