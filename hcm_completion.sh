#! /usr/bin/env bash

_hcm()
{
    local cur_word
    local cards
    local cmds

    cmd="${COMP_WORDS[1]}"
    arg="${COMP_WORDS[COMP_CWORD]#\"}"
    cmds="help add del stats update input list"
    filters="\
 owned\
 missing\
 q=0\
 q=1\
 q=2\
 h=Druid\
 h=Hunter\
 h=Mage\
 h=Paladin\
 h=Priest\
 h=Rogue\
 h=Shaman\
 h=Warlock\
 h=Warrior\
 h=Neutral\
 r=Common\
 r=Rare\
 r=Epic\
 r=Legendary\
 s=c\
 s=gvg\
 s=tgt\
 c="

    if [ "$COMP_CWORD" = 1 ] ; then
        COMPREPLY=( $(compgen -W "$cmds" -- ${arg}) )
    elif [ "$COMP_CWORD" -ge 2 ] && [ "$cmd" = "list" -o "$cmd" = "input" ] ; then
        COMPREPLY=( $(compgen -W "$filters" -- ${arg}) )
    elif [ "$COMP_CWORD" -ge 2 ] && [ "$cmd" = "add" -o "$cmd" = "del" ] ; then
        local cards=$(hcm names | egrep -i "^$arg" | sed 's/.*/"\\"&\\""/')
        eval "COMPREPLY=( $cards )"
    else
        COMPREPLY=()
    fi
    return 0
}

complete -F _hcm hcm
