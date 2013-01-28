function git
	switch $PWD
		case $HOME/goldfeld/*
			command git -c user.email=vic@longstorm.org -c user.name="Vic Goldfeld" $argv
		case '**'
			command git $argv
	end
end
