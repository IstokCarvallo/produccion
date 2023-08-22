$PBExportHeader$w_info_rendimiento_prediocuartel.srw
forward
global type w_info_rendimiento_prediocuartel from w_para_informes
end type
type st_2 from statictext within w_info_rendimiento_prediocuartel
end type
type uo_seleprod from uo_seleccion_productor within w_info_rendimiento_prediocuartel
end type
type st_1 from statictext within w_info_rendimiento_prediocuartel
end type
type st_3 from statictext within w_info_rendimiento_prediocuartel
end type
type uo_selepred from uo_seleccion_prodpredio within w_info_rendimiento_prediocuartel
end type
type uo_selecuar from uo_seleccion_prodcuarteles within w_info_rendimiento_prediocuartel
end type
type st_4 from statictext within w_info_rendimiento_prediocuartel
end type
type st_5 from statictext within w_info_rendimiento_prediocuartel
end type
type st_6 from statictext within w_info_rendimiento_prediocuartel
end type
type st_7 from statictext within w_info_rendimiento_prediocuartel
end type
type em_fecini from editmask within w_info_rendimiento_prediocuartel
end type
type em_fecter from editmask within w_info_rendimiento_prediocuartel
end type
type st_8 from statictext within w_info_rendimiento_prediocuartel
end type
type cbx_consfec from checkbox within w_info_rendimiento_prediocuartel
end type
end forward

global type w_info_rendimiento_prediocuartel from w_para_informes
integer width = 2697
integer height = 1584
string title = "INFORME DE RENDIMIENTO POR PREDIO/CUARTEL"
boolean maxbox = false
boolean resizable = false
st_2 st_2
uo_seleprod uo_seleprod
st_1 st_1
st_3 st_3
uo_selepred uo_selepred
uo_selecuar uo_selecuar
st_4 st_4
st_5 st_5
st_6 st_6
st_7 st_7
em_fecini em_fecini
em_fecter em_fecter
st_8 st_8
cbx_consfec cbx_consfec
end type
global w_info_rendimiento_prediocuartel w_info_rendimiento_prediocuartel

on w_info_rendimiento_prediocuartel.create
int iCurrent
call super::create
this.st_2=create st_2
this.uo_seleprod=create uo_seleprod
this.st_1=create st_1
this.st_3=create st_3
this.uo_selepred=create uo_selepred
this.uo_selecuar=create uo_selecuar
this.st_4=create st_4
this.st_5=create st_5
this.st_6=create st_6
this.st_7=create st_7
this.em_fecini=create em_fecini
this.em_fecter=create em_fecter
this.st_8=create st_8
this.cbx_consfec=create cbx_consfec
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.uo_seleprod
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.uo_selepred
this.Control[iCurrent+6]=this.uo_selecuar
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.st_6
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.em_fecini
this.Control[iCurrent+12]=this.em_fecter
this.Control[iCurrent+13]=this.st_8
this.Control[iCurrent+14]=this.cbx_consfec
end on

on w_info_rendimiento_prediocuartel.destroy
call super::destroy
destroy(this.st_2)
destroy(this.uo_seleprod)
destroy(this.st_1)
destroy(this.st_3)
destroy(this.uo_selepred)
destroy(this.uo_selecuar)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.em_fecini)
destroy(this.em_fecter)
destroy(this.st_8)
destroy(this.cbx_consfec)
end on

event open;call super::open;Boolean	lb_cerrar = False

IF IsNull(uo_seleprod.Codigo) THEN lb_cerrar = True
IF IsNull(uo_selepred.Codigo) THEN lb_cerrar = True
IF IsNull(uo_selecuar.Codigo) THEN lb_cerrar = True

IF lb_cerrar THEN
	Close(This)
ELSE
	uo_seleprod.seleccion(True, False)
	uo_selepred.seleccion(True, False)
	uo_selecuar.seleccion(True, False)
	
	uo_seleprod.Filtra(-1)
	
	em_fecter.Text	=	String(Today(), 'dd/mm/yyyy')
	em_fecini.Text	=	String(RelativeDate(Today(),  -365), 'dd/mm/yyyy')
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_rendimiento_prediocuartel
end type

type st_computador from w_para_informes`st_computador within w_info_rendimiento_prediocuartel
end type

type st_usuario from w_para_informes`st_usuario within w_info_rendimiento_prediocuartel
end type

type st_temporada from w_para_informes`st_temporada within w_info_rendimiento_prediocuartel
end type

type p_logo from w_para_informes`p_logo within w_info_rendimiento_prediocuartel
end type

type st_titulo from w_para_informes`st_titulo within w_info_rendimiento_prediocuartel
integer width = 1925
string text = "RENDIMIENTO POR PREDIO/CUARTEL"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_rendimiento_prediocuartel
integer x = 2258
integer y = 892
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
Date		ld_fecini, ld_fecter
Long		fila
Integer	li_consfecha
String	ls_texto

istr_info.titulo	= "INFORME DE RENDIMIENTO POR PREDIO/CUARTEL"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

ls_texto		=	'Desde ' + em_fecini.Text + ' Hasta ' + em_fecter.Text
ld_fecini	=	Date(em_fecini.Text)
ld_fecter	=	Date(em_fecter.Text)

IF cbx_consfec.Checked THEN
	li_consfecha	=	1
	ls_texto			=	ls_texto + ', Fechas Consolidadas.'
ELSE
	li_consfecha	=	0
	ls_texto			=	ls_texto + ', Abierto por fecha.'
END IF

vinf.dw_1.DataObject = "dw_informe_rendimiento_prediocuartel"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_seleprod.Codigo, uo_selepred.Codigo, uo_selecuar.Codigo, &
								  ld_fecini, ld_fecter, li_consfecha)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.Titulo.Text 	=	ls_texto
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_rendimiento_prediocuartel
integer x = 2258
integer y = 1176
end type

type st_2 from statictext within w_info_rendimiento_prediocuartel
integer x = 457
integer y = 552
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type uo_seleprod from uo_seleccion_productor within w_info_rendimiento_prediocuartel
integer x = 951
integer y = 460
integer taborder = 40
boolean bringtotop = true
end type

on uo_seleprod.destroy
call uo_seleccion_productor::destroy
end on

event ue_cambio;call super::ue_cambio;IF Not IsNull(This.Codigo) OR This.Codigo > 0 THEN
	uo_selepred.Filtra(This.Codigo)	
	uo_selepred.Enabled	=	True
END IF
end event

type st_1 from statictext within w_info_rendimiento_prediocuartel
integer x = 242
integer y = 692
integer width = 1925
integer height = 496
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_rendimiento_prediocuartel
integer x = 242
integer y = 424
integer width = 1925
integer height = 268
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selepred from uo_seleccion_prodpredio within w_info_rendimiento_prediocuartel
integer x = 951
integer y = 732
integer height = 184
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
end type

on uo_selepred.destroy
call uo_seleccion_prodpredio::destroy
end on

event ue_cambio;call super::ue_cambio;IF Not IsNull(This.Codigo) OR This.Codigo > 0 THEN
	uo_selecuar.Filtra(uo_seleprod.Codigo, This.Codigo)
	uo_selecuar.Enabled	=	True
END IF
end event

type uo_selecuar from uo_seleccion_prodcuarteles within w_info_rendimiento_prediocuartel
integer x = 951
integer y = 952
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
end type

on uo_selecuar.destroy
call uo_seleccion_prodcuarteles::destroy
end on

type st_4 from statictext within w_info_rendimiento_prediocuartel
integer x = 242
integer y = 1188
integer width = 1925
integer height = 268
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_rendimiento_prediocuartel
integer x = 457
integer y = 820
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Predio"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_rendimiento_prediocuartel
integer x = 457
integer y = 1040
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cuartel"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_rendimiento_prediocuartel
integer x = 315
integer y = 1296
integer width = 183
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Inicio"
boolean focusrectangle = false
end type

type em_fecini from editmask within w_info_rendimiento_prediocuartel
integer x = 494
integer y = 1284
integer width = 494
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;IF Date(This.Text) > Date(em_fecter.Text) THEN
	MessageBox("Error", "La fecha inicial no debe ser superior a la fecha de termino", &	
					StopSign!)
	This.Text	=	String(RelativeDate(Today(),  -365), 'dd/mm/yyyy')
END IF
end event

type em_fecter from editmask within w_info_rendimiento_prediocuartel
integer x = 1266
integer y = 1284
integer width = 494
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;IF Date(This.Text) < Date(em_fecini.Text) THEN
	MessageBox("Error", "La fecha de termino no puede ser inferior a la fecha de inicio", &
					StopSign!)
	This.Text	=	String(Today(), 'DD/MM/YYYY')
END IF
end event

type st_8 from statictext within w_info_rendimiento_prediocuartel
integer x = 1006
integer y = 1296
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Termino"
boolean focusrectangle = false
end type

type cbx_consfec from checkbox within w_info_rendimiento_prediocuartel
integer x = 1774
integer y = 1288
integer width = 375
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolida"
end type

