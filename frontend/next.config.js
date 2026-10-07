const withBundleAnalyzer = require("@next/bundle-analyzer")({
  enabled: process.env.ANALYZE === "true",
});

/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "standalone",
  poweredByHeader: false,
  images: { unoptimized: true },
  reactStrictMode: false,
  skipTrailingSlashRedirect: true,
  // Repository pages name their forge since multi-forge support; the old
  // forge-less URLs (bookmarks, links in READMEs) are github.com's. Temporary
  // (307) on purpose: browsers cache a permanent redirect for good, so a
  // rollback would leave them on /repo/github/... with no page to serve it.
  redirects: async () => [
    {
      source: "/repo/:owner/:repo",
      destination: "/repo/github/:owner/:repo",
      permanent: false,
    },
  ],
  rewrites: () => {
    if (process.env.GARNIX_SERVER_ORIGIN != null) {
      return [
        {
          source: "/api/:path*",
          destination: `${process.env.GARNIX_SERVER_ORIGIN}/api/:path*`,
        },
      ];
    } else {
      return [];
    }
  },
  generateBuildId: async () => {
    return process.env.NEXT_BUILD_ID || "next-build";
  },
  webpack: (config, { buildId }) => {
    config.resolve.symlinks = false;
    config.output.filename = config.output.filename.replace(
      "[chunkhash]",
      `g-${buildId}`,
    );
    config.module.rules.push({
      test: /\.cast/,
      loader: "raw-loader",
    });
    config.module.rules.push({
      test: /\.wasm/,
      type: "asset/resource",
      generator: {
        filename: "static/chunks/[path][name].[hash][ext]",
      },
    });
    return config;
  },
};

module.exports = withBundleAnalyzer(nextConfig);
