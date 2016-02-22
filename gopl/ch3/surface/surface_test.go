package surface

import (
	"fmt"
	"testing"
)

func TestStyleString(t *testing.T) {
	string := StyleString(0x0000ff)
	if string != "style='fill:#0000ff'" {
		t.Error("styleString error")
	}

	string = StyleString(0xff0000)
	if string != "style='fill:#ff0000'" {
		t.Error("styleString error")
	}
}

func TestColor(t *testing.T) {
	// innermost peak (not the centre spike)
	color := colorPolygon(401.32497224277927, 157.52837447743394,
		398.726896031426, 154.04829162163432,
		396.1288198200727, 154.62013171681858,
		398.726896031426, 157.59178144012935)
	fmt.Printf("color of crest: %d\n", color)
	color = colorPolygon(1, 1, 1, 1, 2, 2, 2, 2)
	fmt.Printf("color of crest: %d\n", color)

	// trough
	color = colorPolygon(274.01923788646684, 246.64469713027432,
		271.4211616751135, 244.8177178357754,
		268.8230854637602, 246.68954709635562,
		271.4211616751135, 248.03545383365784)
	fmt.Printf("color of crest: %d\n", color)
}
