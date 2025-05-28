import {
  type Descriptor,
  getDirectories,
} from "wasi:filesystem/preopens@0.2.5";
import { subscribeDuration } from "wasi:clocks/monotonic-clock@0.2.5";

export function descriptor() {
  const [[descriptor, _]] = getDirectories();
  return descriptor;
}

export function readFileSync(
  path: string,
  _encoding: string,
  dirfd?: Descriptor,
) {
  dirfd ??= descriptor();
  const fd = dirfd.openAt({}, path, {}, { read: true });
  const [bytes, _] = fd.read(10000n, 0n);
  return new TextDecoder().decode(bytes);
}

export function readdirSync(path: string, dirfd?: Descriptor) {
  dirfd ??= descriptor().openAt({}, path, { directory: true }, {
    read: true,
    mutateDirectory: true,
  });
  let dirent;
  const iter = dirfd.readDirectory();
  while (
    (dirent = iter.readDirectoryEntry()) && dirent?.type !== "regular-file" ||
    dirent?.name.startsWith("knowledge")
  );
  return [dirent?.name, dirfd] as const;
}

export function rmSync(path: string, dirfd?: Descriptor) {
  dirfd ??= descriptor();
  dirfd.unlinkFileAt(path);
}

export function writeFileSync(path: string, content: string) {
  const fd = descriptor().openAt({}, path, { create: true, truncate: true }, {
    write: true,
  });
  const bytes = new TextEncoder().encode(content);
  fd.write(bytes, 0n);
}

export { subscribeDuration };
