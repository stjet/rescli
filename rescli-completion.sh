#/usr/bin/env bash

#needs to be source'd

_rescli_completions() {
  #length must be three
  if [ "${#COMP_WORDS[@]}" != "3" ]; then
    return
  fi
  #must be view or delete
  if [[ "${COMP_WORDS[1]}" != "view" && "${COMP_WORDS[1]}" != "delete" ]]; then
    return
  fi
  #autocomplete suggestion
  COMPREPLY=($(compgen -W "$(scheme --script get_uuids.scm)" "${COMP_WORDS[2]}"))
}

complete -F _rescli_completions ./rescli
#complete -W "now never" ./rescli

