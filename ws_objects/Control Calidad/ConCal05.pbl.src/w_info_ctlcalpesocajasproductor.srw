$PBExportHeader$w_info_ctlcalpesocajasproductor.srw
$PBExportComments$Ventana de Informe de Peso Promedio por Racimo
forward
global type w_info_ctlcalpesocajasproductor from w_para_informes
end type
type gb_7 from groupbox within w_info_ctlcalpesocajasproductor
end type
type gb_6 from groupbox within w_info_ctlcalpesocajasproductor
end type
type em_desde from editmask within w_info_ctlcalpesocajasproductor
end type
type em_hasta from editmask within w_info_ctlcalpesocajasproductor
end type
type st_5 from statictext within w_info_ctlcalpesocajasproductor
end type
type st_6 from statictext within w_info_ctlcalpesocajasproductor
end type
type gb_3 from groupbox within w_info_ctlcalpesocajasproductor
end type
type st_44 from statictext within w_info_ctlcalpesocajasproductor
end type
type dw_2 from datawindow within w_info_ctlcalpesocajasproductor
end type
type st_1 from statictext within w_info_ctlcalpesocajasproductor
end type
type st_2 from statictext within w_info_ctlcalpesocajasproductor
end type
type st_3 from statictext within w_info_ctlcalpesocajasproductor
end type
type st_4 from statictext within w_info_ctlcalpesocajasproductor
end type
type st_7 from statictext within w_info_ctlcalpesocajasproductor
end type
type st_8 from statictext within w_info_ctlcalpesocajasproductor
end type
type st_9 from statictext within w_info_ctlcalpesocajasproductor
end type
type st_10 from statictext within w_info_ctlcalpesocajasproductor
end type
type cbx_todosfecha from checkbox within w_info_ctlcalpesocajasproductor
end type
type cbx_consfecha from checkbox within w_info_ctlcalpesocajasproductor
end type
type uo_selespecies from uo_seleccion_especie within w_info_ctlcalpesocajasproductor
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_ctlcalpesocajasproductor
end type
type uo_selproductor from uo_seleccion_productor_mod within w_info_ctlcalpesocajasproductor
end type
type uo_selpacking from uo_seleccion_frigopacking_mod within w_info_ctlcalpesocajasproductor
end type
type uo_selzonas from uo_seleccion_zonas_mod within w_info_ctlcalpesocajasproductor
end type
end forward

global type w_info_ctlcalpesocajasproductor from w_para_informes
integer x = 14
integer y = 32
integer width = 3182
integer height = 1872
string title = "Peso Neto de Cajas"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_7 gb_7
gb_6 gb_6
em_desde em_desde
em_hasta em_hasta
st_5 st_5
st_6 st_6
gb_3 gb_3
st_44 st_44
dw_2 dw_2
st_1 st_1
st_2 st_2
st_3 st_3
st_4 st_4
st_7 st_7
st_8 st_8
st_9 st_9
st_10 st_10
cbx_todosfecha cbx_todosfecha
cbx_consfecha cbx_consfecha
uo_selespecies uo_selespecies
uo_selvariedad uo_selvariedad
uo_selproductor uo_selproductor
uo_selpacking uo_selpacking
uo_selzonas uo_selzonas
end type
global w_info_ctlcalpesocajasproductor w_info_ctlcalpesocajasproductor

type variables

end variables

on w_info_ctlcalpesocajasproductor.create
int iCurrent
call super::create
this.gb_7=create gb_7
this.gb_6=create gb_6
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_5=create st_5
this.st_6=create st_6
this.gb_3=create gb_3
this.st_44=create st_44
this.dw_2=create dw_2
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.st_7=create st_7
this.st_8=create st_8
this.st_9=create st_9
this.st_10=create st_10
this.cbx_todosfecha=create cbx_todosfecha
this.cbx_consfecha=create cbx_consfecha
this.uo_selespecies=create uo_selespecies
this.uo_selvariedad=create uo_selvariedad
this.uo_selproductor=create uo_selproductor
this.uo_selpacking=create uo_selpacking
this.uo_selzonas=create uo_selzonas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_7
this.Control[iCurrent+2]=this.gb_6
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.em_hasta
this.Control[iCurrent+5]=this.st_5
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.gb_3
this.Control[iCurrent+8]=this.st_44
this.Control[iCurrent+9]=this.dw_2
this.Control[iCurrent+10]=this.st_1
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.st_3
this.Control[iCurrent+13]=this.st_4
this.Control[iCurrent+14]=this.st_7
this.Control[iCurrent+15]=this.st_8
this.Control[iCurrent+16]=this.st_9
this.Control[iCurrent+17]=this.st_10
this.Control[iCurrent+18]=this.cbx_todosfecha
this.Control[iCurrent+19]=this.cbx_consfecha
this.Control[iCurrent+20]=this.uo_selespecies
this.Control[iCurrent+21]=this.uo_selvariedad
this.Control[iCurrent+22]=this.uo_selproductor
this.Control[iCurrent+23]=this.uo_selpacking
this.Control[iCurrent+24]=this.uo_selzonas
end on

on w_info_ctlcalpesocajasproductor.destroy
call super::destroy
destroy(this.gb_7)
destroy(this.gb_6)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.gb_3)
destroy(this.st_44)
destroy(this.dw_2)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.cbx_todosfecha)
destroy(this.cbx_consfecha)
destroy(this.uo_selespecies)
destroy(this.uo_selvariedad)
destroy(this.uo_selproductor)
destroy(this.uo_selpacking)
destroy(this.uo_selzonas)
end on

event open;Boolean	lb_Cerrar

If IsNull(uo_SelZonas.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelPacking.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_SelEspecies.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelZonas.Seleccion(True, True)
	uo_SelProductor.Seleccion(True, True)
	uo_SelPacking.Seleccion(True, True)
	uo_SelEspecies.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, True)
	
	uo_SelEspecies.Codigo = 11
	uo_SelEspecies.dw_Seleccion.Object.Codigo[1] = 11
	uo_SelVariedad.Filtra(11)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_ctlcalpesocajasproductor
integer x = 2807
integer y = 268
end type

type st_computador from w_para_informes`st_computador within w_info_ctlcalpesocajasproductor
end type

type st_usuario from w_para_informes`st_usuario within w_info_ctlcalpesocajasproductor
end type

type st_temporada from w_para_informes`st_temporada within w_info_ctlcalpesocajasproductor
end type

type p_logo from w_para_informes`p_logo within w_info_ctlcalpesocajasproductor
end type

type st_titulo from w_para_informes`st_titulo within w_info_ctlcalpesocajasproductor
integer x = 283
integer y = 272
integer width = 2373
string text = "Informe de Peso Neto de Cajas "
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_ctlcalpesocajasproductor
string tag = "Imprimir Reporte"
integer x = 2802
integer y = 568
integer taborder = 210
end type

event pb_acepta::clicked;Integer	li_fila, li_fill, li_filas, li_ConsFechaEmb
Date		ld_FechaEmbaini, ld_FechaEmbafin

SetPointer(HourGlass!)

istr_info.titulo	= 'PESO NETO DE CAJAS'

OpenWithParm(vinf,istr_info)

IF cbx_todosfecha.Checked THEN
	ld_FechaEmbaini =	Date(19000101)
	ld_FechaEmbafin =	Today()
ELSEIF cbx_consfecha.Checked THEN
	li_ConsFechaEmb	= 1
	ld_FechaEmbaini =	Date(19000101)
	ld_FechaEmbafin =	Today()
ELSE
	ld_FechaEmbaini = Date(em_desde.Text)
	ld_FechaEmbafin = Date(em_hasta.Text)
END IF	

If uo_SelEspecies.Codigo = 11 Then
  	vinf.dw_1.DataObject = "dw_info_cajapesofinal"
Else
	vinf.dw_1.DataObject = "dw_info_cajapesofinal_esp"
End If

vinf.dw_1.SetTransObject(sqlca)
  
li_fila= vinf.dw_1.Retrieve(gi_codexport, uo_SelZonas.Codigo, uo_SelProductor.Codigo, uo_SelPacking.Codigo, uo_SelEspecies.Codigo,&
	                         uo_SelVariedad.Codigo,ld_FechaEmbaini,ld_FechaEmbafin,li_ConsFechaEmb)

If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No Existen pesos fuera de rango para su selección"+ &
 					"~n",StopSign!, Ok!)	
End If
	
F_Membrete(vinf.dw_1)
vinf.dw_1.Modify("zona_cod.text = '" + string(uo_SelZonas.Codigo) + "'")
vinf.dw_1.Modify("zona.text = '" + uo_SelZonas.Nombre+ "'")
vinf.dw_1.Modify("prod_cod.text = '" + string(uo_SelProductor.Codigo) + "'")
vinf.dw_1.Modify("prod.text = '" + uo_SelProductor.Nombre + "'")
vinf.dw_1.Modify("cod_espe.text = '" + string(uo_SelEspecies.Codigo) + "'")
vinf.dw_1.Modify("especie.text = '" + uo_SelEspecies.Nombre + "'")
vinf.dw_1.Modify("packing_cod.text = '" + string(uo_SelPacking.Codigo) + "'")
vinf.dw_1.Modify("packing.text = '" + uo_SelPacking.Nombre + "'")
vinf.dw_1.Modify("desde.text = '" + em_desde.text + "'")
vinf.dw_1.Modify("hasta.text = '" + em_hasta.text + "'")	

	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
	
SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_ctlcalpesocajasproductor
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2807
integer y = 844
integer taborder = 220
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_7 from groupbox within w_info_ctlcalpesocajasproductor
integer x = 439
integer y = 1320
integer width = 2043
integer height = 248
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type gb_6 from groupbox within w_info_ctlcalpesocajasproductor
integer x = 439
integer y = 520
integer width = 2043
integer height = 784
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type em_desde from editmask within w_info_ctlcalpesocajasproductor
integer x = 969
integer y = 1428
integer width = 352
integer height = 88
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_hasta from editmask within w_info_ctlcalpesocajasproductor
integer x = 1467
integer y = 1428
integer width = 352
integer height = 88
integer taborder = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_5 from statictext within w_info_ctlcalpesocajasproductor
integer x = 978
integer y = 1368
integer width = 279
integer height = 52
boolean bringtotop = true
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

type st_6 from statictext within w_info_ctlcalpesocajasproductor
integer x = 1467
integer y = 1368
integer width = 215
integer height = 52
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

type gb_3 from groupbox within w_info_ctlcalpesocajasproductor
integer x = 357
integer y = 476
integer width = 2213
integer height = 1140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_44 from statictext within w_info_ctlcalpesocajasproductor
integer x = 279
integer y = 460
integer width = 2373
integer height = 1208
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_info_ctlcalpesocajasproductor
boolean visible = false
integer x = 2693
integer y = 1160
integer width = 411
integer height = 400
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_ctlcapesocajaproductor"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_info_ctlcalpesocajasproductor
integer x = 1806
integer y = 560
integer width = 256
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
string text = "Todos"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_ctlcalpesocajasproductor
integer x = 2066
integer y = 560
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
string text = "Consolidado"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_ctlcalpesocajasproductor
integer x = 503
integer y = 648
integer width = 462
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
string text = "Zona Origen"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_ctlcalpesocajasproductor
integer x = 503
integer y = 784
integer width = 297
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

type st_7 from statictext within w_info_ctlcalpesocajasproductor
integer x = 503
integer y = 1056
integer width = 297
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

type st_8 from statictext within w_info_ctlcalpesocajasproductor
integer x = 503
integer y = 920
integer width = 297
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
string text = "Packing"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_ctlcalpesocajasproductor
integer x = 503
integer y = 1192
integer width = 297
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

type st_10 from statictext within w_info_ctlcalpesocajasproductor
integer x = 475
integer y = 1424
integer width = 485
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
string text = "Fecha Embalaje"
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_info_ctlcalpesocajasproductor
integer x = 1902
integer y = 1444
integer width = 96
integer height = 60
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = " "
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_desde.Enabled		=	False
	em_hasta.Enabled	=	False
	cbx_consfecha.checked	=	FALSE
ELSE
	em_desde.Enabled	   =	TRUE
	em_hasta.Enabled	=	TRUE
	em_desde.Setfocus()
END IF

RETURN 0
end event

type cbx_consfecha from checkbox within w_info_ctlcalpesocajasproductor
integer x = 2098
integer y = 1432
integer width = 192
integer height = 80
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = " "
end type

event clicked;IF This.Checked THEN
	em_desde.Enabled		=	False
	em_hasta.Enabled			=	False
	cbx_todosfecha.checked	= FALSE
ELSE
	em_desde.Enabled	   =	TRUE
	em_hasta.Enabled			=	TRUE
	em_desde.Setfocus()
END IF

RETURN 0
end event

type uo_selespecies from uo_seleccion_especie within w_info_ctlcalpesocajasproductor
event destroy ( )
integer x = 983
integer y = 1036
integer height = 84
integer taborder = 130
boolean bringtotop = true
end type

on uo_selespecies.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelVariedad.Filtra(-1)
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		
End Choose
end event

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_ctlcalpesocajasproductor
event destroy ( )
integer x = 983
integer y = 1160
integer taborder = 140
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selproductor from uo_seleccion_productor_mod within w_info_ctlcalpesocajasproductor
event destroy ( )
integer x = 983
integer y = 752
integer taborder = 220
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

type uo_selpacking from uo_seleccion_frigopacking_mod within w_info_ctlcalpesocajasproductor
event destroy ( )
integer x = 983
integer y = 888
integer taborder = 230
boolean bringtotop = true
end type

on uo_selpacking.destroy
call uo_seleccion_frigopacking_mod::destroy
end on

type uo_selzonas from uo_seleccion_zonas_mod within w_info_ctlcalpesocajasproductor
event destroy ( )
integer x = 983
integer y = 616
integer taborder = 220
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelPacking.Filtra(-1)
		
	Case Else
		uo_SelPacking.Filtra(This.Codigo)
		
End Choose
end event

