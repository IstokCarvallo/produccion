$PBExportHeader$w_info_cumplimiento.srw
forward
global type w_info_cumplimiento from w_para_informes
end type
type st_2 from statictext within w_info_cumplimiento
end type
type st_3 from statictext within w_info_cumplimiento
end type
type st_5 from statictext within w_info_cumplimiento
end type
type st_7 from statictext within w_info_cumplimiento
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_cumplimiento
end type
type uo_selespecie from uo_seleccion_especie within w_info_cumplimiento
end type
type uo_selzonas from uo_seleccion_zonas within w_info_cumplimiento
end type
type st_4 from statictext within w_info_cumplimiento
end type
type uo_seltemporada from uo_seleccion_paramtemporada within w_info_cumplimiento
end type
type dw_1 from uo_dw within w_info_cumplimiento
end type
type st_1 from statictext within w_info_cumplimiento
end type
type st_10 from statictext within w_info_cumplimiento
end type
type uo_selproductor from uo_seleccion_productor within w_info_cumplimiento
end type
type cbx_agrupa from checkbox within w_info_cumplimiento
end type
type uo_selgrupo from uo_seleccion_grupoeconomico within w_info_cumplimiento
end type
type st_6 from statictext within w_info_cumplimiento
end type
type st_11 from statictext within w_info_cumplimiento
end type
type em_semana from editmask within w_info_cumplimiento
end type
type em_desde from editmask within w_info_cumplimiento
end type
type rb_semanal from radiobutton within w_info_cumplimiento
end type
type rb_diario from radiobutton within w_info_cumplimiento
end type
type ddlb_colores from dropdownlistbox within w_info_cumplimiento
end type
type st_8 from statictext within w_info_cumplimiento
end type
type cbx_todos from checkbox within w_info_cumplimiento
end type
end forward

global type w_info_cumplimiento from w_para_informes
integer width = 3739
integer height = 1744
string title = "Cumplimientos"
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
uo_selproductor uo_selproductor
cbx_agrupa cbx_agrupa
uo_selgrupo uo_selgrupo
st_6 st_6
st_11 st_11
em_semana em_semana
em_desde em_desde
rb_semanal rb_semanal
rb_diario rb_diario
ddlb_colores ddlb_colores
st_8 st_8
cbx_todos cbx_todos
end type
global w_info_cumplimiento w_info_cumplimiento

type variables
DataWindowChild idwc_tempo
Integer ii_Color = -1
end variables

on w_info_cumplimiento.create
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
this.uo_selproductor=create uo_selproductor
this.cbx_agrupa=create cbx_agrupa
this.uo_selgrupo=create uo_selgrupo
this.st_6=create st_6
this.st_11=create st_11
this.em_semana=create em_semana
this.em_desde=create em_desde
this.rb_semanal=create rb_semanal
this.rb_diario=create rb_diario
this.ddlb_colores=create ddlb_colores
this.st_8=create st_8
this.cbx_todos=create cbx_todos
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
this.Control[iCurrent+13]=this.uo_selproductor
this.Control[iCurrent+14]=this.cbx_agrupa
this.Control[iCurrent+15]=this.uo_selgrupo
this.Control[iCurrent+16]=this.st_6
this.Control[iCurrent+17]=this.st_11
this.Control[iCurrent+18]=this.em_semana
this.Control[iCurrent+19]=this.em_desde
this.Control[iCurrent+20]=this.rb_semanal
this.Control[iCurrent+21]=this.rb_diario
this.Control[iCurrent+22]=this.ddlb_colores
this.Control[iCurrent+23]=this.st_8
this.Control[iCurrent+24]=this.cbx_todos
end on

on w_info_cumplimiento.destroy
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
destroy(this.uo_selproductor)
destroy(this.cbx_agrupa)
destroy(this.uo_selgrupo)
destroy(this.st_6)
destroy(this.st_11)
destroy(this.em_semana)
destroy(this.em_desde)
destroy(this.rb_semanal)
destroy(this.rb_diario)
destroy(this.ddlb_colores)
destroy(this.st_8)
destroy(this.cbx_todos)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelProductor.codigo)		THEN lb_Cerrar	=	True
IF IsNull(uo_SelZonas.codigo)			THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.codigo)			THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.codigo)		THEN lb_Cerrar	=	True
IF IsNull(uo_SelTemporada.codigo)	THEN lb_Cerrar	=	True
IF IsNull(uo_SelGrupo.codigo)	THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelProductor.Seleccion(True, False)
	uo_SelZonas.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelTemporada.Seleccion(False, False)
	uo_SelGrupo.Seleccion(True, False)
	
	uo_SelTemporada.Inicia(gstr_tempo.temporada)
	uo_SelEspecie.Inicia(11)
	uo_SelVariedad.Filtra(11)
	uo_SelProductor.Filtra(-1)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_cumplimiento
boolean visible = true
integer x = 3310
integer y = 700
integer taborder = 90
integer weight = 400
fontcharset fontcharset = ansi!
boolean enabled = true
end type

event pb_excel::clicked;call super::clicked;SetPointer(HourGlass!)

Byte		lb_Agrupa
Long		ll_fila
string 	ls_path, ls_file
Date		ld_desde, ld_hasta

If rb_Semanal.Checked Then 
	dw_1.DataObject =	"dw_info_cumplimiento_semanal"
Else
	dw_1.DataObject =	"dw_info_cumplimiento_dia"
End If

dw_1.SetTransObject(sqlca)

If cbx_Agrupa.Checked Then lb_Agrupa = 1
ld_desde	=	Date(em_desde.Text)

ll_fila = dw_1.Retrieve(uo_SelTemporada.Codigo, Integer(em_semana.Text), ld_desde, uo_SelZonas.Codigo, uo_SelGrupo.Codigo, &
					uo_SelProductor.Codigo,  uo_SelEspecie.Codigo, uo_Selvariedad.Codigo, lb_Agrupa, ii_Color)

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

type st_computador from w_para_informes`st_computador within w_info_cumplimiento
end type

type st_usuario from w_para_informes`st_usuario within w_info_cumplimiento
end type

type st_temporada from w_para_informes`st_temporada within w_info_cumplimiento
end type

type p_logo from w_para_informes`p_logo within w_info_cumplimiento
end type

type st_titulo from w_para_informes`st_titulo within w_info_cumplimiento
integer y = 292
integer width = 2880
string text = "Informe Cumplimiento"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cumplimiento
integer x = 3310
integer y = 404
integer taborder = 80
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Byte	lb_Agrupa = 0
Long	ll_fila
Date	ld_desde, ld_hasta

istr_info.titulo	= "Cumpolimiento"
istr_info.copias	= 1

If cbx_Agrupa.Checked Then lb_Agrupa = 1
ld_desde	=	Date(em_desde.Text)

OpenWithParm(vinf, istr_info)
If rb_Semanal.Checked Then 
	vinf.dw_1.DataObject =	"dw_info_cumplimiento_semanal"
Else
	vinf.dw_1.DataObject =	"dw_info_cumplimiento_dia"
End If

vinf.dw_1.SetTransObject(sqlca)

ll_fila = vinf.dw_1.Retrieve(uo_SelTemporada.Codigo, Integer(em_semana.Text), ld_desde, uo_SelZonas.Codigo, uo_SelGrupo.Codigo, &
					uo_SelProductor.Codigo,  uo_SelEspecie.Codigo, uo_Selvariedad.Codigo, lb_Agrupa, ii_Color)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_cumplimiento
integer x = 3310
integer y = 1004
integer taborder = 100
end type

type st_2 from statictext within w_info_cumplimiento
integer x = 306
integer y = 824
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

type st_3 from statictext within w_info_cumplimiento
integer x = 306
integer y = 624
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

type st_5 from statictext within w_info_cumplimiento
integer x = 1797
integer y = 824
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

type st_7 from statictext within w_info_cumplimiento
integer x = 306
integer y = 1024
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

type uo_selvariedad from uo_seleccion_variedad within w_info_cumplimiento
event destroy ( )
string tag = "Seleccion de Variedad"
integer x = 672
integer y = 948
integer height = 164
integer taborder = 30
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_cumplimiento
event destroy ( )
string tag = "Seleccion de Especie"
integer x = 672
integer y = 820
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

type uo_selzonas from uo_seleccion_zonas within w_info_cumplimiento
event destroy ( )
string tag = "Seleccion de Zonas"
integer x = 2162
integer y = 528
integer height = 164
integer taborder = 40
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelProductor.Filtra(-1)
		
	Case Else
		uo_SelProductor.Filtra(This.Codigo)

End Choose
end event

type st_4 from statictext within w_info_cumplimiento
integer x = 1797
integer y = 624
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

type uo_seltemporada from uo_seleccion_paramtemporada within w_info_cumplimiento
string tag = "Seleccion de Temporada"
integer x = 672
integer y = 616
integer height = 80
integer taborder = 10
boolean bringtotop = true
end type

on uo_seltemporada.destroy
call uo_seleccion_paramtemporada::destroy
end on

type dw_1 from uo_dw within w_info_cumplimiento
boolean visible = false
integer x = 3314
integer y = 1344
integer width = 251
integer height = 168
integer taborder = 0
boolean bringtotop = true
string dataobject = "dw_info_cumplimiento_semanal"
boolean vscrollbar = false
end type

type st_1 from statictext within w_info_cumplimiento
integer x = 251
integer y = 440
integer width = 1449
integer height = 972
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

type st_10 from statictext within w_info_cumplimiento
integer x = 1797
integer y = 1024
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
string text = "Grupo"
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_productor within w_info_cumplimiento
integer x = 2162
integer y = 728
integer taborder = 50
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		
	CASE ELSE
		uo_SelZonas.Todos(False)
		uo_SelZonas.Inicia(This.Zona)

END CHOOSE
end event

type cbx_agrupa from checkbox within w_info_cumplimiento
integer x = 2619
integer y = 520
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

type uo_selgrupo from uo_seleccion_grupoeconomico within w_info_cumplimiento
integer x = 2162
integer y = 940
integer taborder = 60
boolean bringtotop = true
end type

on uo_selgrupo.destroy
call uo_seleccion_grupoeconomico::destroy
end on

type st_6 from statictext within w_info_cumplimiento
integer x = 1705
integer y = 440
integer width = 1449
integer height = 972
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

type st_11 from statictext within w_info_cumplimiento
integer x = 1797
integer y = 1168
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
string text = "Semana"
boolean focusrectangle = false
end type

type em_semana from editmask within w_info_cumplimiento
string tag = "Numero de Semana"
integer x = 2162
integer y = 1156
integer width = 174
integer height = 88
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "##"
end type

event getfocus;This.SelectText(1, 2)


end event

event modified;DataStore	lds_semana
Integer		li_Semana

lds_semana 		= Create DataStore

lds_semana.DataObject = 'dw_consulta_numerosemana'
lds_semana.SetTransObject(SQLCA)

lds_semana.Retrieve(Integer(This.Text), uo_SelTemporada.Codigo, uo_SelEspecie.Codigo)
li_Semana	=	F_NroSemanaAno(Date(string(lds_semana.Object.fech_dlunes[1],'dd/mm/yyyy')))

If Integer(This.Text) = 53 Then
	If 	li_Semana = 52 Then
		This.Text = ''
		MessageBox('Alerta', 'Este año no posee semana 53.', Exclamation!, OK!)
		Return 
	End If
End If

If lds_semana.Rowcount() > 0 Then 	em_desde.Text = string(lds_semana.Object.fech_dlunes[1], 'dd/mm/yyyy')

Destroy lds_semana
end event

type em_desde from editmask within w_info_cumplimiento
integer x = 2624
integer y = 1156
integer width = 434
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type rb_semanal from radiobutton within w_info_cumplimiento
integer x = 1797
integer y = 1292
integer width = 457
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
string text = "Semanal"
boolean checked = true
end type

type rb_diario from radiobutton within w_info_cumplimiento
integer x = 2601
integer y = 1292
integer width = 457
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
string text = "Diario"
end type

type ddlb_colores from dropdownlistbox within w_info_cumplimiento
integer x = 672
integer y = 1240
integer width = 878
integer height = 376
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 16777215
boolean enabled = false
boolean sorted = false
boolean vscrollbar = true
string item[] = {"Blancas","Verdes","Rojas","Negras","Rojas C/Semilla","Negras C/Semilla","Tricolor"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_Color = Index
end event

type st_8 from statictext within w_info_cumplimiento
integer x = 306
integer y = 1256
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
string text = "Color"
boolean focusrectangle = false
end type

type cbx_todos from checkbox within w_info_cumplimiento
integer x = 695
integer y = 1136
integer width = 457
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
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then 
	ii_Color = -1
	ddlb_colores.Enabled = False
Else
	uo_SelVariedad.Todos(True)
	ddlb_colores.Enabled = True
End If
end event

