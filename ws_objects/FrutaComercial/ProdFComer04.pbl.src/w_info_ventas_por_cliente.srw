$PBExportHeader$w_info_ventas_por_cliente.srw
$PBExportComments$Informe de Existencia de Fruta Comercial en Camara por Productor y Calibre.
forward
global type w_info_ventas_por_cliente from w_para_informes
end type
type gb_detalle from groupbox within w_info_ventas_por_cliente
end type
type st_2 from statictext within w_info_ventas_por_cliente
end type
type st_4 from statictext within w_info_ventas_por_cliente
end type
type st_5 from statictext within w_info_ventas_por_cliente
end type
type uo_selvari from uo_seleccion_variedad within w_info_ventas_por_cliente
end type
type st_6 from statictext within w_info_ventas_por_cliente
end type
type rb_cliente from radiobutton within w_info_ventas_por_cliente
end type
type rb_guia from radiobutton within w_info_ventas_por_cliente
end type
type gb_ordena from groupbox within w_info_ventas_por_cliente
end type
type uo_selespe from uo_seleccion_especie within w_info_ventas_por_cliente
end type
type st_11 from statictext within w_info_ventas_por_cliente
end type
type st_13 from statictext within w_info_ventas_por_cliente
end type
type em_desde from editmask within w_info_ventas_por_cliente
end type
type em_hasta from editmask within w_info_ventas_por_cliente
end type
type st_14 from statictext within w_info_ventas_por_cliente
end type
type em_guia from editmask within w_info_ventas_por_cliente
end type
type cbx_fectodos from checkbox within w_info_ventas_por_cliente
end type
type cbx_feccons from checkbox within w_info_ventas_por_cliente
end type
type cbx_todosguia from checkbox within w_info_ventas_por_cliente
end type
type cbx_consguia from checkbox within w_info_ventas_por_cliente
end type
type gb_3 from groupbox within w_info_ventas_por_cliente
end type
type st_10 from statictext within w_info_ventas_por_cliente
end type
type rb_infprod from radiobutton within w_info_ventas_por_cliente
end type
type rb_infcli from radiobutton within w_info_ventas_por_cliente
end type
type gb_informe from groupbox within w_info_ventas_por_cliente
end type
type st_12 from statictext within w_info_ventas_por_cliente
end type
type rb_grupo from radiobutton within w_info_ventas_por_cliente
end type
type rb_detalle from radiobutton within w_info_ventas_por_cliente
end type
type uo_selecli from uo_seleccion_clientesprod within w_info_ventas_por_cliente
end type
type uo_seleent from uo_seleccion_clienprove within w_info_ventas_por_cliente
end type
type st_3 from statictext within w_info_ventas_por_cliente
end type
type st_8 from statictext within w_info_ventas_por_cliente
end type
type st_1 from statictext within w_info_ventas_por_cliente
end type
type ddlb_tipodocto from dropdownlistbox within w_info_ventas_por_cliente
end type
end forward

global type w_info_ventas_por_cliente from w_para_informes
integer x = 0
integer y = 0
integer width = 3680
integer height = 1552
string title = "Informe de Ventas por Cliente"
boolean maxbox = false
gb_detalle gb_detalle
st_2 st_2
st_4 st_4
st_5 st_5
uo_selvari uo_selvari
st_6 st_6
rb_cliente rb_cliente
rb_guia rb_guia
gb_ordena gb_ordena
uo_selespe uo_selespe
st_11 st_11
st_13 st_13
em_desde em_desde
em_hasta em_hasta
st_14 st_14
em_guia em_guia
cbx_fectodos cbx_fectodos
cbx_feccons cbx_feccons
cbx_todosguia cbx_todosguia
cbx_consguia cbx_consguia
gb_3 gb_3
st_10 st_10
rb_infprod rb_infprod
rb_infcli rb_infcli
gb_informe gb_informe
st_12 st_12
rb_grupo rb_grupo
rb_detalle rb_detalle
uo_selecli uo_selecli
uo_seleent uo_seleent
st_3 st_3
st_8 st_8
st_1 st_1
ddlb_tipodocto ddlb_tipodocto
end type
global w_info_ventas_por_cliente w_info_ventas_por_cliente

type variables
str_mant				istr_mant
DataWindowChild	idwc_cliente
Integer				ii_tipo
end variables

forward prototypes
public function boolean wf_existe_guia (integer ai_codigo, long al_guia)
end prototypes

public function boolean wf_existe_guia (integer ai_codigo, long al_guia);Long ll_guia

SELECT Distinct mfco_guisii
INTO   :ll_guia
FROM   dbo.spro_movtofrutacomenca 
WHERE  :ai_codigo in (-1,-9,clie_codigo)
AND    :al_guia in (-1,-9,mfco_guisii);

IF IsNull(ll_guia) OR ll_guia = 0 THEN
	MessageBox("Atención","Guía No Existe, Ingrese Otra")
	RETURN FALSE
ELSEIF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Spro_movtofrutacomenca")
	RETURN FALSE
END IF
RETURN TRUE
end function

on w_info_ventas_por_cliente.create
int iCurrent
call super::create
this.gb_detalle=create gb_detalle
this.st_2=create st_2
this.st_4=create st_4
this.st_5=create st_5
this.uo_selvari=create uo_selvari
this.st_6=create st_6
this.rb_cliente=create rb_cliente
this.rb_guia=create rb_guia
this.gb_ordena=create gb_ordena
this.uo_selespe=create uo_selespe
this.st_11=create st_11
this.st_13=create st_13
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_14=create st_14
this.em_guia=create em_guia
this.cbx_fectodos=create cbx_fectodos
this.cbx_feccons=create cbx_feccons
this.cbx_todosguia=create cbx_todosguia
this.cbx_consguia=create cbx_consguia
this.gb_3=create gb_3
this.st_10=create st_10
this.rb_infprod=create rb_infprod
this.rb_infcli=create rb_infcli
this.gb_informe=create gb_informe
this.st_12=create st_12
this.rb_grupo=create rb_grupo
this.rb_detalle=create rb_detalle
this.uo_selecli=create uo_selecli
this.uo_seleent=create uo_seleent
this.st_3=create st_3
this.st_8=create st_8
this.st_1=create st_1
this.ddlb_tipodocto=create ddlb_tipodocto
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_detalle
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_5
this.Control[iCurrent+5]=this.uo_selvari
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.rb_cliente
this.Control[iCurrent+8]=this.rb_guia
this.Control[iCurrent+9]=this.gb_ordena
this.Control[iCurrent+10]=this.uo_selespe
this.Control[iCurrent+11]=this.st_11
this.Control[iCurrent+12]=this.st_13
this.Control[iCurrent+13]=this.em_desde
this.Control[iCurrent+14]=this.em_hasta
this.Control[iCurrent+15]=this.st_14
this.Control[iCurrent+16]=this.em_guia
this.Control[iCurrent+17]=this.cbx_fectodos
this.Control[iCurrent+18]=this.cbx_feccons
this.Control[iCurrent+19]=this.cbx_todosguia
this.Control[iCurrent+20]=this.cbx_consguia
this.Control[iCurrent+21]=this.gb_3
this.Control[iCurrent+22]=this.st_10
this.Control[iCurrent+23]=this.rb_infprod
this.Control[iCurrent+24]=this.rb_infcli
this.Control[iCurrent+25]=this.gb_informe
this.Control[iCurrent+26]=this.st_12
this.Control[iCurrent+27]=this.rb_grupo
this.Control[iCurrent+28]=this.rb_detalle
this.Control[iCurrent+29]=this.uo_selecli
this.Control[iCurrent+30]=this.uo_seleent
this.Control[iCurrent+31]=this.st_3
this.Control[iCurrent+32]=this.st_8
this.Control[iCurrent+33]=this.st_1
this.Control[iCurrent+34]=this.ddlb_tipodocto
end on

on w_info_ventas_por_cliente.destroy
call super::destroy
destroy(this.gb_detalle)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.uo_selvari)
destroy(this.st_6)
destroy(this.rb_cliente)
destroy(this.rb_guia)
destroy(this.gb_ordena)
destroy(this.uo_selespe)
destroy(this.st_11)
destroy(this.st_13)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_14)
destroy(this.em_guia)
destroy(this.cbx_fectodos)
destroy(this.cbx_feccons)
destroy(this.cbx_todosguia)
destroy(this.cbx_consguia)
destroy(this.gb_3)
destroy(this.st_10)
destroy(this.rb_infprod)
destroy(this.rb_infcli)
destroy(this.gb_informe)
destroy(this.st_12)
destroy(this.rb_grupo)
destroy(this.rb_detalle)
destroy(this.uo_selecli)
destroy(this.uo_seleent)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.st_1)
destroy(this.ddlb_tipodocto)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SeleCli.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_SeleEnt.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_SelEspe.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_SelVari.Codigo) Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelVari.Todos(True)
	uo_SelVari.cbx_Todos.Enabled	=	False
	
	em_desde.text 	= '01/01/' + String(Year(Today()))
	em_Hasta.text 	= String(Today(),'dd/mm/yyyy')
	ii_tipo				=	-1
	istr_mant.Argumento[1]	=	''
	ddlb_tipodocto.SelectItem(3)
End If

end event

type pb_excel from w_para_informes`pb_excel within w_info_ventas_por_cliente
integer x = 3177
integer y = 364
end type

type st_computador from w_para_informes`st_computador within w_info_ventas_por_cliente
integer x = 1006
integer width = 2583
end type

type st_usuario from w_para_informes`st_usuario within w_info_ventas_por_cliente
integer x = 1006
integer width = 2583
end type

type st_temporada from w_para_informes`st_temporada within w_info_ventas_por_cliente
integer x = 1006
integer width = 2583
end type

type p_logo from w_para_informes`p_logo within w_info_ventas_por_cliente
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_ventas_por_cliente
integer x = 123
integer width = 3008
string text = "Ventas por Cliente"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ventas_por_cliente
integer x = 3177
integer y = 664
integer taborder = 150
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila, ll_guia
Integer	li_agrupa
Date		ld_fecini, ld_fecter

istr_info.titulo	= "VENTAS POR CLIENTES"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

If rb_infcli.checked Then
	vinf.dw_1.DataObject = "dw_info_ventas_por_cliente"
Else
	vinf.dw_1.DataObject = "dw_info_ventas_por_productor"
End If

If rb_cliente.Checked AND rb_infcli.checked Then
	vinf.dw_1.DataObject = "dw_info_ventas_por_cliente_ordprod"
End If

vinf.dw_1.SetTransObject(sqlca)

//Guía Despacho
If istr_mant.argumento[1] = "" Then
	MessageBox("Atención","Debe ingresar una Guía de despacho")
	em_guia.SetFocus()
	Return
Else
	ll_guia   = Long(istr_mant.argumento[1])
End If

//Fecha Movimiento
If em_desde.text = '00/00/0000' OR em_hasta.text = '00/00/0000' Then
	MessageBox("Atención","Debe ingresar fechas de movimiento")
	Return
ElseIf Date(em_hasta.text) <= Date(em_desde.text) Then
	MessageBox("Error de consistencia","Fecha termino no puede ser menor a fecha inicio",StopSign!)
	em_hasta.text = ""
	em_hasta.SetFocus()
	Return
Else	
	ld_fecini = Date(em_desde.text)
	ld_fecter = Date(em_hasta.text)
End If

If rb_grupo.Checked Then
	li_agrupa = 1
ElseIf rb_detalle.Checked Then
	li_agrupa = 2
End If

ll_Fila	=	vinf.dw_1.Retrieve(uo_SeleCli.Codigo,uo_SelEspe.Codigo,uo_SelVari.Codigo,ll_guia,&
                               ld_fecini,ld_fecter,li_agrupa, uo_SeleEnt.Codigo,ii_tipo)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos :~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("fechadesde.text = '" + String(ld_fecini,'dd/mm/yyyy') + "'")
	vinf.dw_1.ModIfy("fechahasta.text = '" + String(ld_fecter,'dd/mm/yyyy') + "'")
	vinf.dw_1.Object.DataWindow.Zoom = 90	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_ventas_por_cliente
integer x = 3177
integer y = 1028
integer taborder = 160
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type gb_detalle from groupbox within w_info_ventas_por_cliente
integer x = 2711
integer y = 1072
integer width = 379
integer height = 280
integer taborder = 140
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Bins"
end type

type st_2 from statictext within w_info_ventas_por_cliente
integer x = 183
integer y = 740
integer width = 421
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Documento"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_ventas_por_cliente
integer x = 1746
integer y = 788
integer width = 320
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type st_5 from statictext within w_info_ventas_por_cliente
integer x = 1746
integer y = 964
integer width = 279
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type uo_selvari from uo_seleccion_variedad within w_info_ventas_por_cliente
integer x = 2130
integer y = 864
integer height = 172
integer taborder = 70
boolean bringtotop = true
end type

on uo_selvari.destroy
call uo_seleccion_variedad::destroy
end on

type st_6 from statictext within w_info_ventas_por_cliente
integer x = 183
integer y = 568
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type rb_cliente from radiobutton within w_info_ventas_por_cliente
integer x = 2199
integer y = 1144
integer width = 439
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean checked = true
end type

type rb_guia from radiobutton within w_info_ventas_por_cliente
integer x = 2199
integer y = 1236
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Guía Despacho"
end type

type gb_ordena from groupbox within w_info_ventas_por_cliente
integer x = 2162
integer y = 1072
integer width = 539
integer height = 280
integer taborder = 130
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Ordenamiento"
end type

type uo_selespe from uo_seleccion_especie within w_info_ventas_por_cliente
integer x = 2130
integer y = 676
integer taborder = 60
end type

on uo_selespe.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio();IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_SelVari.Todos(True)
		
		uo_SelVari.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_SelVari.Filtra(This.Codigo)
		
		uo_SelVari.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type st_11 from statictext within w_info_ventas_por_cliente
integer x = 1705
integer y = 420
integer width = 1426
integer height = 636
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

type st_13 from statictext within w_info_ventas_por_cliente
integer x = 338
integer y = 1232
integer width = 210
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type em_desde from editmask within w_info_ventas_por_cliente
integer x = 631
integer y = 1220
integer width = 288
integer height = 88
integer taborder = 100
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_hasta from editmask within w_info_ventas_por_cliente
integer x = 1225
integer y = 1220
integer width = 288
integer height = 88
integer taborder = 110
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_14 from statictext within w_info_ventas_por_cliente
integer x = 1024
integer y = 1232
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Hasta"
boolean focusrectangle = false
end type

type em_guia from editmask within w_info_ventas_por_cliente
integer x = 631
integer y = 932
integer width = 261
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "############"
end type

event modified;If Not IsNull(em_guia.text) AND Not em_guia.text = "" Then
	If Not wf_Existe_guia(uo_SeleCli.Codigo,Long(This.text)) Then
		This.Text = ""
		istr_mant.argumento[1] = ""
		This.SetFocus()
		Return 1
	Else
		istr_mant.argumento[1] = This.text
	End If
End If

end event

type cbx_fectodos from checkbox within w_info_ventas_por_cliente
integer x = 626
integer y = 1132
integer width = 343
integer height = 72
integer taborder = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	em_desde.Enabled	=	FALSE
	em_hasta.Enabled	=	FALSE
    	em_desde.text   	=	'01/01/1900'
	em_hasta.text   	=	String(Today())
	cbx_feccons.enabled   =  TRUE
	cbx_feccons.checked   =  FALSE
ELSE
	em_desde.Enabled	=	TRUE
	em_hasta.Enabled	=	TRUE
	em_desde.SetFocus()
END IF
RETURN 0

end event

type cbx_feccons from checkbox within w_info_ventas_por_cliente
integer x = 1134
integer y = 1132
integer width = 384
integer height = 72
integer taborder = 90
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;IF This.Checked THEN
	em_desde.Enabled	=	FALSE
	em_hasta.Enabled	=	FALSE
	em_desde.text   	=	'01/01/1900'
	em_hasta.text   	=	String(Today())
	cbx_fectodos.Checked = False
ELSE
	em_desde.Enabled	=	TRUE
	em_hasta.Enabled	=	TRUE
	em_desde.SetFocus()
END IF
RETURN 0

end event

type cbx_todosguia from checkbox within w_info_ventas_por_cliente
integer x = 626
integer y = 848
integer width = 343
integer height = 72
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[1] = '-1'
	cbx_consguia.Enabled = FALSE
	em_guia.Enabled = FALSE
ELSE
	cbx_consguia.Enabled = TRUE
	em_guia.Enabled = TRUE
	istr_mant.argumento[1] = ''
END IF
end event

type cbx_consguia from checkbox within w_info_ventas_por_cliente
integer x = 1106
integer y = 852
integer width = 384
integer height = 72
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[1] = '-9'
	cbx_todosguia.Enabled = FALSE
	em_guia.Enabled = FALSE
ELSE
	cbx_todosguia.Enabled = TRUE
	em_guia.Enabled = TRUE
	istr_mant.argumento[1] = ''
END IF
end event

type gb_3 from groupbox within w_info_ventas_por_cliente
integer x = 197
integer y = 1068
integer width = 1431
integer height = 284
integer taborder = 170
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Movimiento"
end type

type st_10 from statictext within w_info_ventas_por_cliente
integer x = 137
integer y = 1056
integer width = 1568
integer height = 328
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

type rb_infprod from radiobutton within w_info_ventas_por_cliente
integer x = 1769
integer y = 1240
integer width = 338
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
end type

event clicked;rb_cliente.Checked 	= 	False
rb_guia.Checked 		= 	False
rb_cliente.Enabled 	= 	False
rb_guia.Enabled 		= 	False
gb_ordena.Enabled	=	False
end event

type rb_infcli from radiobutton within w_info_ventas_por_cliente
integer x = 1783
integer y = 1148
integer width = 320
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean checked = true
end type

event clicked;rb_cliente.Checked 	= 	True
rb_cliente.Enabled 	= 	True
rb_guia.Enabled 		= 	True
gb_ordena.Enabled	=	True
end event

type gb_informe from groupbox within w_info_ventas_por_cliente
integer x = 1751
integer y = 1072
integer width = 402
integer height = 280
integer taborder = 120
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Informe"
end type

type st_12 from statictext within w_info_ventas_por_cliente
integer x = 1705
integer y = 1056
integer width = 1426
integer height = 328
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

type rb_grupo from radiobutton within w_info_ventas_por_cliente
integer x = 2757
integer y = 1148
integer width = 315
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Grupo"
boolean checked = true
end type

type rb_detalle from radiobutton within w_info_ventas_por_cliente
integer x = 2757
integer y = 1240
integer width = 315
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Detalle"
end type

type uo_selecli from uo_seleccion_clientesprod within w_info_ventas_por_cliente
event destroy ( )
integer x = 626
integer y = 460
integer height = 172
integer taborder = 10
boolean bringtotop = true
end type

on uo_selecli.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_seleent from uo_seleccion_clienprove within w_info_ventas_por_cliente
event destroy ( )
integer x = 2130
integer y = 456
integer width = 869
integer taborder = 20
boolean bringtotop = true
end type

on uo_seleent.destroy
call uo_seleccion_clienprove::destroy
end on

type st_3 from statictext within w_info_ventas_por_cliente
integer x = 1746
integer y = 568
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Entidad"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_ventas_por_cliente
integer x = 183
integer y = 940
integer width = 411
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Documento"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_ventas_por_cliente
integer x = 137
integer y = 420
integer width = 1568
integer height = 636
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

type ddlb_tipodocto from dropdownlistbox within w_info_ventas_por_cliente
integer x = 626
integer y = 708
integer width = 896
integer height = 352
integer taborder = 70
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean sorted = false
string item[] = {"Guia Despacho","Boleta","Ambos"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;If index = 1 Then
	ii_Tipo = 7
ElseIf Index = 2 Then
	ii_Tipo = 9
Else
	ii_Tipo = -1
End If
end event

