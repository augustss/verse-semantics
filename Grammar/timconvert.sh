sed < TimGrammar.txt -n -e '/^Alpha/,/^File/p' | \
    sed -e '/^[ 	]*$/d' | \
    sed -e ':a;N;$!ba;s/\n[ 	]*|/|/g' | \
    sed -e '/^U8/d' -e '/^UTF8/d' -e '/^UTF32/d' | \
    sed -e "s/^Printable.*/Printable := 0o09 | ' '..'~'/" | \
    sed -e "s/^File.*/File      := List end/" | \
    sed -e 's/^List.*/List      := Scan [Commas {Separator Commas} [Separator]]/' | \
    sed -e 's/0o/#x/g' | \
    sed -e '/^Ind/d' -e '/^Ded/d' -e '/^Line/d' -e '/[ 	]*if/d' -e '/[ 	]*else/d' | \
    sed -e "s/'''/'\"'/g" | \
    sed -e 's/ := / ::= /' | \
    sed -e "s/'{'/%%%</" -e "s/'}'/%%%>/" -e "s/'\['/%%%(/" -e "s/']'/%%%)/" | \
    sed -e 's/{/(/g' -e 's/}/)*/g' -e 's/\[/(/g' -e 's/]/)?/g' | \
    sed -e "s/%%%</'{'/" -e "s/%%%>/'}'/" -e "s/%%%(/'['/" -e "s/%%%)/']'/" | \
    sed -e "s/'\(.\)'\.\.'\(.\)'/[\1-\2]/g" | \
    sed -e "s/\(#x..\)\.\.\(#x..\)/[\1-\2]/g" | \
    sed -e "s/\([^']\)&/\1 AT /g" | \
    sed -e "s/\([^']\)!/\1 NOTAT /g" | \
    #    sed -e 's/^Ampersand.*/Ampersand ::= Space Def (';'|Ending)/' | \
    sed -e '/^Ampersand/d' | \
    cat
