$PBExportHeader$w_info_estimacionsemanal_cc.srw
$PBExportComments$Informe con estimcion semanal
forward
global type w_info_estimacionsemanal_cc from w_para_informes
end type
type st_2 from statictext within w_info_estimacionsemanal_cc
end type
type st_3 from statictext within w_info_estimacionsemanal_cc
end type
type st_5 from statictext within w_info_estimacionsemanal_cc
end type
type st_7 from statictext within w_info_estimacionsemanal_cc
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_estimacionsemanal_cc
end type
type uo_selespecie from uo_seleccion_especie within w_info_estimacionsemanal_cc
end type
type uo_selzonas from uo_seleccion_zonas within w_info_estimacionsemanal_cc
end type
type st_4 from statictext within w_info_estimacionsemanal_cc
end type
type st_6 from statictext within w_info_estimacionsemanal_cc
end type
type cbx_todo from checkbox within w_info_estimacionsemanal_cc
end type
type em_hasta from editmask within w_info_estimacionsemanal_cc
end type
type st_8 from statictext within w_info_estimacionsemanal_cc
end type
type uo_seltemporada from uo_seleccion_paramtemporada within w_info_estimacionsemanal_cc
end type
type dw_1 from uo_dw within w_info_estimacionsemanal_cc
end type
type st_11 from statictext within w_info_estimacionsemanal_cc
end type
type em_semana2 from editmask within w_info_estimacionsemanal_cc
end type
type st_12 from statictext within w_info_estimacionsemanal_cc
end type
type em_desde from editmask within w_info_estimacionsemanal_cc
end type
type em_semana1 from editmask within w_info_estimacionsemanal_cc
end type
type st_1 from statictext within w_info_estimacionsemanal_cc
end type
type st_10 from statictext within w_info_estimacionsemanal_cc
end type
type uo_selpredios from uo_seleccion_predios within w_info_estimacionsemanal_cc
end type
type uo_selproductor from uo_seleccion_productor within w_info_estimacionsemanal_cc
end type
end forward

global type w_info_estimacionsemanal_cc from w_para_informes
integer width = 2240
integer height = 2092
string title = "Informe Estimcion Semanal por Calibres"
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
st_6 st_6
cbx_todo cbx_todo
em_hasta em_hasta
st_8 st_8
uo_seltemporada uo_seltemporada
dw_1 dw_1
st_11 st_11
em_semana2 em_semana2
st_12 st_12
em_desde em_desde
em_semana1 em_semana1
st_1 st_1
st_10 st_10
uo_selpredios uo_selpredios
uo_selproductor uo_selproductor
end type
global w_info_estimacionsemanal_cc w_info_estimacionsemanal_cc

type variables
DataWindowChild idwc_tempo
end variables

forward prototypes
public function boolean wf_cargasemana ()
end prototypes

public function boolean wf_cargasemana ();	DataStore	lds_semana
	Boolean		lb_Retorno = True
	
	lds_semana 		= Create DataStore
	
	lds_semana.DataObject = 'dw_consulta_numerosemana'
	lds_semana.SetTransObject(SQLCA)
	
	lds_semana.Retrieve(40, uo_SelTemporada.Codigo, uo_SelEspecie.Codigo)
	
	If lds_semana.Rowcount() > 0 Then 	
		lds_semana.Retrieve(lds_semana.Object.iniciotemp[1], uo_SelTemporada.Codigo, uo_SelEspecie.Codigo)
		em_semana1.Text	= String(lds_semana.Object.iniciotemp[1])
		em_desde.Text	= String(lds_semana.Object.fech_dlunes[1],'dd/mm/yyyy')
		lds_semana.Retrieve(lds_semana.Object.iniciotemp[1]-1, uo_SelTemporada.Codigo, uo_SelEspecie.Codigo)
		em_semana2.Text	= String(lds_semana.Object.iniciotemp[1] - 1)
		em_hasta.Text		= String(lds_semana.Object.fech_dlunes[1],'dd/mm/yyyy')
	Else
		lb_Retorno = False
	End If
	
	Destroy lds_semana
	
	Return lb_Retorno
end function

on w_info_estimacionsemanal_cc.create
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
this.st_6=create st_6
this.cbx_todo=create cbx_todo
this.em_hasta=create em_hasta
this.st_8=create st_8
this.uo_seltemporada=create uo_seltemporada
this.dw_1=create dw_1
this.st_11=create st_11
this.em_semana2=create em_semana2
this.st_12=create st_12
this.em_desde=create em_desde
this.em_semana1=create em_semana1
this.st_1=create st_1
this.st_10=create st_10
this.uo_selpredios=create uo_selpredios
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_7
this.Control[iCurrent+5]=this.uo_selvariedad
this.Control[iCurrent+6]=this.uo_selespecie
this.Control[iCurrent+7]=this.uo_selzonas
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.st_6
this.Control[iCurrent+10]=this.cbx_todo
this.Control[iCurrent+11]=this.em_hasta
this.Control[iCurrent+12]=this.st_8
this.Control[iCurrent+13]=this.uo_seltemporada
this.Control[iCurrent+14]=this.dw_1
this.Control[iCurrent+15]=this.st_11
this.Control[iCurrent+16]=this.em_semana2
this.Control[iCurrent+17]=this.st_12
this.Control[iCurrent+18]=this.em_desde
this.Control[iCurrent+19]=this.em_semana1
this.Control[iCurrent+20]=this.st_1
this.Control[iCurrent+21]=this.st_10
this.Control[iCurrent+22]=this.uo_selpredios
this.Control[iCurrent+23]=this.uo_selproductor
end on

on w_info_estimacionsemanal_cc.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.st_7)
destroy(this.uo_selvariedad)
destroy(this.uo_selespecie)
destroy(this.uo_selzonas)
destroy(this.st_4)
destroy(this.st_6)
destroy(this.cbx_todo)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.uo_seltemporada)
destroy(this.dw_1)
destroy(this.st_11)
destroy(this.em_semana2)
destroy(this.st_12)
destroy(this.em_desde)
destroy(this.em_semana1)
destroy(this.st_1)
destroy(this.st_10)
destroy(this.uo_selpredios)
destroy(this.uo_selproductor)
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
	uo_SelProductor.Seleccion(True, True)
	uo_SelZonas.Seleccion(True, True)
	uo_SelVariedad.Seleccion(True, True)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelTemporada.Seleccion(False, False)
	
	uo_SelTemporada.Inicia(gstr_tempo.temporada)
	uo_SelEspecie.Inicia(11)
	uo_SelVariedad.Filtra(11)
	uo_SelProductor.Filtra(-1)
	
	wf_CargaSemana()
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_estimacionsemanal_cc
boolean visible = true
integer x = 1797
integer y = 752
integer taborder = 120
boolean enabled = true
end type

event pb_excel::clicked;call super::clicked;SetPointer(HourGlass!)

Long		ll_fila, ll_cierre
string 	ls_path, ls_file
Date		ld_desde, ld_hasta

dw_1.SetTransObject(sqlca)

ld_desde	=	Date(em_desde.Text)
ld_hasta	=	Date(em_hasta.Text)

ll_fila = dw_1.Retrieve(uo_SelEspecie.Codigo, uo_Selvariedad.Codigo, uo_SelZonas.Codigo, &
							uo_Selproductor.Codigo, uo_SelPredios.Codigo, uo_SelTemporada.Codigo, &
							ld_desde, ld_hasta, Integer(em_semana1.Text))

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

type st_computador from w_para_informes`st_computador within w_info_estimacionsemanal_cc
end type

type st_usuario from w_para_informes`st_usuario within w_info_estimacionsemanal_cc
end type

type st_temporada from w_para_informes`st_temporada within w_info_estimacionsemanal_cc
end type

type p_logo from w_para_informes`p_logo within w_info_estimacionsemanal_cc
end type

type st_titulo from w_para_informes`st_titulo within w_info_estimacionsemanal_cc
integer x = 219
integer y = 292
integer width = 1449
string text = "Informe Estimacion Semanal por Calibres"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_estimacionsemanal_cc
integer x = 1797
integer y = 456
integer taborder = 110
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long	ll_fila, ll_Cierre
Date	ld_desde, ld_hasta

istr_info.titulo	= "Estimcion Semanal por Calibre"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

ld_desde	=	Date(em_desde.Text)
ld_hasta	=	Date(em_hasta.Text)

vinf.dw_1.DataObject =	"dw_info_comp_estimacionsemanal_Cali"
vinf.dw_1.Object.DataWindow.Zoom 	=	85
vinf.dw_1.SetTransObject(sqlca)

ll_fila = vinf.dw_1.Retrieve(uo_SelEspecie.Codigo, uo_Selvariedad.Codigo, uo_SelZonas.Codigo, &
								  uo_SelProductor.Codigo, uo_SelPredios.Codigo, uo_SelTemporada.Codigo, ld_desde, ld_hasta, &
								  Integer(em_Semana1.Text))

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF
end event

type pb_salir from w_para_informes`pb_salir within w_info_estimacionsemanal_cc
integer x = 1797
integer y = 1056
integer taborder = 130
end type

type st_2 from statictext within w_info_estimacionsemanal_cc
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

type st_3 from statictext within w_info_estimacionsemanal_cc
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

type st_5 from statictext within w_info_estimacionsemanal_cc
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

type st_7 from statictext within w_info_estimacionsemanal_cc
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

type uo_selvariedad from uo_seleccion_variedad within w_info_estimacionsemanal_cc
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

type uo_selespecie from uo_seleccion_especie within w_info_estimacionsemanal_cc
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

type uo_selzonas from uo_seleccion_zonas within w_info_estimacionsemanal_cc
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

type st_4 from statictext within w_info_estimacionsemanal_cc
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

type st_6 from statictext within w_info_estimacionsemanal_cc
integer x = 306
integer y = 1708
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

type cbx_todo from checkbox within w_info_estimacionsemanal_cc
integer x = 1134
integer y = 1480
integer width = 274
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;If This.Checked Then 
	If Not wf_cargasemana() Then MessageBox('Error', 'No se pudieron cargar las fechas de las semanas.')
Else
	em_desde.Text	= ''
	em_hasta.Text		= ''
	em_semana1.Text	= ''
	em_semana2.Text	= ''
End If
end event

type em_hasta from editmask within w_info_estimacionsemanal_cc
integer x = 1134
integer y = 1696
integer width = 434
integer height = 88
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_8 from statictext within w_info_estimacionsemanal_cc
integer x = 864
integer y = 1708
integer width = 302
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
string text = "Hasta"
boolean focusrectangle = false
end type

type uo_seltemporada from uo_seleccion_paramtemporada within w_info_estimacionsemanal_cc
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

type dw_1 from uo_dw within w_info_estimacionsemanal_cc
boolean visible = false
integer x = 1801
integer y = 1396
integer width = 251
integer height = 168
integer taborder = 0
boolean bringtotop = true
string dataobject = "dw_gene_estimacionsemanal_cali_excel"
boolean vscrollbar = false
end type

type st_11 from statictext within w_info_estimacionsemanal_cc
integer x = 306
integer y = 1588
integer width = 347
integer height = 64
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

type em_semana2 from editmask within w_info_estimacionsemanal_cc
string tag = "Numero de Semana"
integer x = 672
integer y = 1692
integer width = 174
integer height = 88
integer taborder = 90
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

event modified;DataStore 	lds_semana
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

If lds_semana.Rowcount() > 0 Then 	em_hasta.Text = string(lds_semana.Object.fech_dlunes[1],'dd/mm/yyyy')

Destroy lds_semana
end event

type st_12 from statictext within w_info_estimacionsemanal_cc
integer x = 864
integer y = 1588
integer width = 302
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_estimacionsemanal_cc
integer x = 1134
integer y = 1576
integer width = 434
integer height = 88
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_semana1 from editmask within w_info_estimacionsemanal_cc
string tag = "Numero de Semana"
integer x = 672
integer y = 1576
integer width = 174
integer height = 88
integer taborder = 70
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

event getfocus;This.SelectText(1, 2)


end event

type st_1 from statictext within w_info_estimacionsemanal_cc
integer x = 251
integer y = 440
integer width = 1449
integer height = 1412
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

type st_10 from statictext within w_info_estimacionsemanal_cc
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

type uo_selpredios from uo_seleccion_predios within w_info_estimacionsemanal_cc
integer x = 672
integer y = 1316
integer height = 160
integer taborder = 60
boolean bringtotop = true
end type

on uo_selpredios.destroy
call uo_seleccion_predios::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_estimacionsemanal_cc
integer x = 672
integer y = 1108
integer taborder = 50
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelPredios.Filtra(This.Codigo)

End Choose
end event

