type obj

type collidable = Player of obj | Block of obj

val check_collision : collidable -> collidable -> bool

val check_on_screen : collidable -> bool
