import type { Metadata } from "next";
import "./globals.css";

export const metadata: Metadata = {
  title: "DNS Regressor Compilation Explorer",
  description: "Inspect how DNS record sets compile into rules, dependencies, and unifiers.",
  icons: { icon: "/favicon.svg", shortcut: "/favicon.svg" },
};

export default function RootLayout({ children }: Readonly<{ children: React.ReactNode }>) {
  return <html lang="en"><body>{children}</body></html>;
}
