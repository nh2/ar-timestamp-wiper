ar-timestamp-wiper
==================

This application takes an archive (`.a`) file as created by the Unix `ar` tool, and sets all time stamps in it to zero.

These time stamps make ar generate different output for same input every time, which is problematic if you want to know if the contents actually changed as compared to the last time you created the archive (e.g. in build tools to avoid unnecessary linking).

Recent versions of GNU binutils and BSD ar af the `-D` flag for deterministic mode that creates the archive with all time stamps set to zero. However, these versions of `ar` are not wide spread yet, and this tool can help with that.
