## Save new entity to Synapse; Folder for dataset manifest, File for others
save_file_to_synapse <- function(syn, synapseclient, name,
                                 parent, annotations) {
  name <- syn_prettify(name)

  # create dummy file to upload to Synapse
  write(name, file = name)
  new_file <- synapseclient$File(
    path = name,
    name = name,
    parent = parent,
    annotations = annotations
  )
  new_file <- syn$store(new_file)

  # remove dummy file
  file.remove(name)

  new_file$id
}


save_folder_to_synapse <- function(syn, synapseclient, name,
                                   parent, annotations) {
  name <- syn_prettify(name)
  new_folder <- synapseclient$Folder(
    name,
    parent = parent,
    annotations = annotations
  )
  new_folder <- syn$store(new_folder)
  new_folder$id
}
