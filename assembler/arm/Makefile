# Rsync targets

PI='pi:/home/tobin/build/'

get:
	rsync -avuzb --exclude '*~' $(PI) .

put:
	rsync -avuzb --exclude '*~' . $(PI)

sync: get put

