% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resource.R
\name{armadillo.upload_resource}
\alias{armadillo.upload_resource}
\title{Uploads a resource to a folder in a project}
\usage{
armadillo.upload_resource(project, folder, resource, name = NULL)
}
\arguments{
\item{project}{the project to upload to}

\item{folder}{the folder to upload to}

\item{resource}{the resource to upload}

\item{name}{name of the resource (optional)}
}
\description{
Uploads a resource to a folder in a project
}
\examples{
\dontrun{
armadillo.upload_table(
  project = "gecko",
  folder = "core_all",
  table1
)
}

}
