/** @module Interface wasi:filesystem/preopens@0.2.3 **/
/**
 * Return the set of preopened directories, and their paths.
 */

import type { Descriptor } from "./wasi-filesystem-types.d.ts";
export function getDirectories(): Array<[Descriptor, string]>;
export type { Descriptor };
