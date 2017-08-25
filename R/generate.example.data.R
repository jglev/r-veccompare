# Save some example vectors into our /data directory, following http://r-pkgs.had.co.nz/data.html:

example.vectors.list <- list(
	"vector_a" = c("a", "b", "c", "d", "z", "q", "x"),
	"vector_b" = c("a", "a", "b", "e", "f", "z"),
	"vector_c" = c("b", "f", "g", "h", "i"),
	"vector_d" = c("c", "i", "b", "k", "l"),
	"vector_e" = c("a", "g", "h", "k", "i"),
	"vector_f" = c("f", "g", "a", "w")
)

# devtools::use_data(example.vectors.list, overwrite = TRUE)
