$PBExportHeader$w_info_fruticola.srw
forward
global type w_info_fruticola from w_para_informes
end type
type st_2 from statictext within w_info_fruticola
end type
type st_3 from statictext within w_info_fruticola
end type
type st_5 from statictext within w_info_fruticola
end type
type st_7 from statictext within w_info_fruticola
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_fruticola
end type
type uo_selespecie from uo_seleccion_especie within w_info_fruticola
end type
type uo_selzonas from uo_seleccion_zonas within w_info_fruticola
end type
type st_4 from statictext within w_info_fruticola
end type
type uo_seltemporada from uo_seleccion_paramtemporada within w_info_fruticola
end type
type dw_1 from uo_dw within w_info_fruticola
end type
type st_1 from statictext within w_info_fruticola
end type
type st_10 from statictext within w_info_fruticola
end type
type uo_selpredios from uo_seleccion_predios within w_info_fruticola
end type
type uo_selproductor from uo_seleccion_productor within w_info_fruticola
end type
type cbx_agrupa from checkbox within w_info_fruticola
end type
end forward

global type w_info_fruticola from w_para_informes
integer width = 2240
integer height = 1752
string title = "Fruticola"
boolean minbox = false
boolean maxbox = false
st_2 st_2
st_3 st_3
st_5 st_5
st_7 st_7
uo_selvariedad uo_selvariedad
uo_selespecie uo_selespecie
uo_selzonas uo_selzonas
st_4 st_4
uo_seltemporada uo_seltemporada
dw_1 dw_1
st_1 st_1
st_10 st_10
uo_selpredios uo_selpredios
uo_selproductor uo_selproductor
cbx_agrupa cbx_agrupa
end type
global w_info_fruticola w_info_fruticola

type variables
DataWindowChild idwc_tempo
end variables

on w_info_fruticola.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_3=create st_3
this.st_5=create st_5
this.st_7=create st_7
this.uo_selvariedad=create uo_selvariedad
this.uo_selespecie=create uo_selespecie
this.uo_selzonas=create uo_selzonas
this.st_4=create st_4
this.uo_seltemporada=create uo_seltemporada
this.dw_1=create dw_1
this.st_1=create st_1
this.st_10=create st_10
this.uo_selpredios=create uo_selpredios
this.uo_selproductor=create uo_selproductor
this.cbx_agrupa=create cbx_agrupa
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_7
this.Control[iCurrent+5]=this.uo_selvariedad
this.Control[iCurrent+6]=this.uo_selespecie
this.Control[iCurrent+7]=this.uo_selzonas
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.uo_seltemporada
this.Control[iCurrent+10]=this.dw_1
this.Control[iCurrent+11]=this.st_1
this.Control[iCurrent+12]=this.st_10
this.Control[iCurrent+13]=this.uo_selpredios
this.Control[iCurrent+14]=this.uo_selproductor
this.Control[iCurrent+15]=this.cbx_agrupa
end on

on w_info_fruticola.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.st_7)
destroy(this.uo_selvariedad)
destroy(this.uo_selespecie)
destroy(this.uo_selzonas)
destroy(this.st_4)
destroy(this.uo_seltemporada)
destroy(this.dw_1)
destroy(this.st_1)
destroy(this.st_10)
destroy(this.uo_selpredios)
destroy(this.uo_selproductor)
destroy(this.cbx_agrupa)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelProductor.codigo)		THEN lb_Cerrar	=	True
IF IsNull(uo_SelZonas.codigo)			THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.codigo)			THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.codigo)		THEN lb_Cerrar	=	True
IF IsNull(uo_SelTemporada.codigo)	THEN lb_Cerrar	=	True


IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelProductor.Seleccion(True, False)
	uo_SelZonas.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelTemporada.Seleccion(False, False)
	
	uo_SelTemporada.Inicia(gstr_tempo.temporada)
	uo_SelEspecie.Inicia(11)
	uo_SelVariedad.Filtra(11)
	uo_SelProductor.Filtra(-1)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_fruticola
boolean visible = true
integer x = 1797
integer y = 752
integer taborder = 120
integer weight = 400
fontcharset fontcharset = ansi!
boolean enabled = true
end type

event pb_excel::clicked;call super::clicked;SetPointer(HourGlass!)

Byte		lb_Agrupa
Long		ll_fila
string 	ls_path, ls_file
Date		ld_desde, ld_hasta

dw_1.SetTransObject(sqlca)

If cbx_Agrupa.Checked Then lb_Agrupa = 1

ll_fila = dw_1.Retrieve(uo_SelTemporada.Codigo, uo_SelProductor.Codigo, uo_SelZonas.Codigo, &
								  uo_SelPredios.Codigo, uo_SelEspecie.Codigo, uo_Selvariedad.Codigo, lb_Agrupa)

If ll_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else

	If GetFileSaveName( "Seleccione archivo",  ls_path, ls_file, "Excel", ".XLS Files (*.xls),*.xls" , "C:\") = -1 Then
		MessageBox('Error', 'No se encontro archivo solicitdo.' , StopSign!, OK! )
		Return -1
	End If

	If dw_1.SaveAs(ls_File, Excel8!, True) = -1 Then
		MessageBox('Error', 'No se pùdo generar archivo ('+ ls_file +') con informción solicitda.' , StopSign!, OK! )
		Return -1
	Else
		MessageBox('Atencion', 'Archivo ('+ ls_file +') generado satisfactoriamente.' , Information!, OK! )
	End If
End If

SetPointer(Arrow!)
end event

type st_computador from w_para_informes`st_computador within w_info_fruticola
end type

type st_usuario from w_para_informes`st_usuario within w_info_fruticola
end type

type st_temporada from w_para_informes`st_temporada within w_info_fruticola
end type

type p_logo from w_para_informes`p_logo within w_info_fruticola
end type

type st_titulo from w_para_informes`st_titulo within w_info_fruticola
integer x = 219
integer y = 292
integer width = 1449
string text = "Informe Fruticola"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_fruticola
integer x = 1797
integer y = 456
integer taborder = 110
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Byte	lb_Agrupa = 0
Long	ll_fila
Date	ld_desde, ld_hasta

istr_info.titulo	= "Fruticola"
istr_info.copias	= 1

If cbx_Agrupa.Checked Then lb_Agrupa = 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject =	"dw_info_fruticola"
vinf.dw_1.Object.DataWindow.Zoom 	=	67
vinf.dw_1.SetTransObject(sqlca)

ll_fila = vinf.dw_1.Retrieve(uo_SelTemporada.Codigo, uo_SelProductor.Codigo, uo_SelZonas.Codigo, &
								  uo_SelPredios.Codigo, uo_SelEspecie.Codigo, uo_Selvariedad.Codigo, lb_Agrupa)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_fruticola
integer x = 1797
integer y = 1056
integer taborder = 130
end type

type st_2 from statictext within w_info_fruticola
integer x = 306
integer y = 612
integer width = 347
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_fruticola
integer x = 306
integer y = 512
integer width = 347
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
string text = "Temporada"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_fruticola
integer x = 306
integer y = 1204
integer width = 347
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

type st_7 from statictext within w_info_fruticola
integer x = 306
integer y = 804
integer width = 325
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
string text = "Variedad"
boolean focusrectangle = false
end type

type uo_selvariedad from uo_seleccion_variedad within w_info_fruticola
event destroy ( )
string tag = "Seleccion de Variedad"
integer x = 672
integer y = 716
integer height = 164
integer taborder = 30
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_fruticola
event destroy ( )
string tag = "Seleccion de Especie"
integer x = 672
integer y = 604
integer height = 76
integer taborder = 20
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		
	CASE ELSE
		uo_SelVariedad.Filtra(This.Codigo)

END CHOOSE
end event

type uo_selzonas from uo_seleccion_zonas within w_info_fruticola
event destroy ( )
string tag = "Seleccion de Zonas"
integer x = 672
integer y = 908
integer height = 164
integer taborder = 40
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

Choose Case This.Codigo
	Case -1, -9
		uo_SelProductor.Filtra(-1)
		
	Case Else
		uo_SelProductor.Filtra(This.Codigo)

End Choose
end event

type st_4 from statictext within w_info_fruticola
integer x = 306
integer y = 1004
integer width = 325
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
string text = "Zona"
boolean focusrectangle = false
end type

type uo_seltemporada from uo_seleccion_paramtemporada within w_info_fruticola
string tag = "Seleccion de Temporada"
integer x = 672
integer y = 500
integer height = 80
integer taborder = 10
boolean bringtotop = true
end type

on uo_seltemporada.destroy
call uo_seleccion_paramtemporada::destroy
end on

type dw_1 from uo_dw within w_info_fruticola
boolean visible = false
integer x = 1801
integer y = 1396
integer width = 251
integer height = 168
integer taborder = 0
boolean bringtotop = true
string dataobject = "dw_info_fruticola"
boolean vscrollbar = false
end type

type st_1 from statictext within w_info_fruticola
integer x = 251
integer y = 440
integer width = 1449
integer height = 1140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_fruticola
integer x = 306
integer y = 1404
integer width = 347
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

type uo_selpredios from uo_seleccion_predios within w_info_fruticola
integer x = 672
integer y = 1316
integer height = 160
integer taborder = 60
boolean bringtotop = true
end type

on uo_selpredios.destroy
call uo_seleccion_predios::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_fruticola
integer x = 672
integer y = 1108
integer taborder = 50
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelPredios.Filtra(This.Codigo)
		uo_SelZonas.Todos(False)
		uo_SelZonas.Inicia(This.Zona)

End Choose
end event

type cbx_agrupa from checkbox within w_info_fruticola
integer x = 1138
integer y = 900
integer width = 402
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
string text = "Agrupa"
end type

