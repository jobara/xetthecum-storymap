unlink("docs/WoL", recursive=TRUE)
R.utils::copyDirectory("WoL", "docs/WoL")
R.utils::copyDirectory("files", "docs/files")
R.utils::copyDirectory("audio", "docs/audio")