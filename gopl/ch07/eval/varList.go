package eval

func (v Var) VarList() []Var {
	return []Var{v}
}

func (l literal) VarList() []Var {
	return []Var{}
}

func (u unary) VarList() []Var {
	return u.x.VarList()
}

func (b binary) VarList() []Var {
	return append(b.x.VarList(), b.y.VarList()...)
}

func (c call) VarList() []Var {
	var vars []Var
	for _, e := range c.args {
		vars = append(vars, e.VarList()...)
	}
	return vars
}
