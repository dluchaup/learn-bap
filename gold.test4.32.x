File                : test4.32.x
Arch                : i386
Entry               : 0x80482F0:32
0x8048298d: .init_proc -> __x86.get_pc_thunk.bx
0x80482add: .init_proc -> .__gmon_start__
0x804830cd: _start -> .__libc_start_main
0x80483afd: __do_global_dtors_aux -> deregister_tm_clones
0x80483e2d: frame_dummy -> register_tm_clones
0x8048412d: uf2 -> uf2
0x804842bd: uf3 -> uf2
0x8048445d: uf1 -> uf2
0x8048455d: uf1 -> uf3
0x8048487d: ft2 -> ft2
0x80484a0d: ft3 -> ft2
0x80484bad: ft1 -> ft2
0x80484cad: ft1 -> ft3
0x80484fcd: f2 -> f2
0x8048516d: f3 -> f2
0x8048527d: f3 -> f2
0x8048544d: f1 -> f2
0x8048554d: f1 -> f3
0x804858ad: main -> f1
0x80485b6d: __libc_csu_init -> __x86.get_pc_thunk.bx
0x80485ced: __libc_csu_init -> .init_proc
0x8048628d: .term_proc -> __x86.get_pc_thunk.bx
{.init_proc{.__gmon_start__[;0x80482AD:32]
__x86.get_pc_thunk.bx[;0x8048298:32]
}
	.term_proc{__x86.get_pc_thunk.bx[;0x8048628:32]
}
	__do_global_dtors_aux{deregister_tm_clones[;0x80483AF:32]
}
	__libc_csu_init{.init_proc[;0x80485CE:32]
__x86.get_pc_thunk.bx[;0x80485B6:32]
}
	_start{.__libc_start_main[;0x804830C:32]
}
	f1{f2[;0x8048544:32]
f3[;0x8048554:32]
}
	f2{f2[;0x80484FC:32]
}
	f3{f2[;0x8048527:32;0x8048516:32]
}
	frame_dummy{register_tm_clones[;0x80483E2:32]
}
	ft1{ft2[;0x80484BA:32]
ft3[;0x80484CA:32]
}
	ft2{ft2[;0x8048487:32]
}
	ft3{ft2[;0x80484A0:32]
}
	main{f1[;0x804858A:32]
}
	uf1{uf2[;0x8048445:32]
uf3[;0x8048455:32]
}
	uf2{uf2[;0x8048412:32]
}
	uf3{uf2[;0x804842B:32]
}
	}
{.__gmon_start__{.init_proc[;0x80482AD:32]
}
	.__libc_start_main{_start[;0x804830C:32]
}
	.init_proc{__libc_csu_init[;0x80485CE:32]
}
	__x86.get_pc_thunk.bx{.init_proc[;0x8048298:32]
.term_proc[;0x8048628:32]
__libc_csu_init[;0x80485B6:32]
}
	deregister_tm_clones{__do_global_dtors_aux[;0x80483AF:32]
}
	f1{main[;0x804858A:32]
}
	f2{f1[;0x8048544:32]
f2[;0x80484FC:32]
f3[;0x8048527:32;0x8048516:32]
}
	f3{f1[;0x8048554:32]
}
	ft2{ft1[;0x80484BA:32]
ft2[;0x8048487:32]
ft3[;0x80484A0:32]
}
	ft3{ft1[;0x80484CA:32]
}
	register_tm_clones{frame_dummy[;0x80483E2:32]
}
	uf2{uf1[;0x8048445:32]
uf2[;0x8048412:32]
uf3[;0x804842B:32]
}
	uf3{uf1[;0x8048455:32]
}
	}
Found 22 symbols
