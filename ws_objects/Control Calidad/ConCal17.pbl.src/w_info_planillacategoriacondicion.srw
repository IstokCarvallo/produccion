$PBExportHeader$w_info_planillacategoriacondicion.srw
$PBExportComments$Ventana de Consulta planilla de recepcion
forward
global type w_info_planillacategoriacondicion from w_para_informes
end type
type em_desde from editmask within w_info_planillacategoriacondicion
end type
type st_12 from statictext within w_info_planillacategoriacondicion
end type
type st_13 from statictext within w_info_planillacategoriacondicion
end type
type em_hasta from editmask within w_info_planillacategoriacondicion
end type
type st_33 from statictext within w_info_planillacategoriacondicion
end type
type cbx_todosfecha from checkbox within w_info_planillacategoriacondicion
end type
type st_14 from statictext within w_info_planillacategoriacondicion
end type
type st_2 from statictext within w_info_planillacategoriacondicion
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_planillacategoriacondicion
end type
type st_3 from statictext within w_info_planillacategoriacondicion
end type
type st_8 from statictext within w_info_planillacategoriacondicion
end type
type gb_3 from groupbox within w_info_planillacategoriacondicion
end type
type uo_selproductor from uo_seleccion_productor_mod within w_info_planillacategoriacondicion
end type
type st_44 from statictext within w_info_planillacategoriacondicion
end type
type uo_selespecies from uo_seleccion_especie within w_info_planillacategoriacondicion
end type
type st_6 from statictext within w_info_planillacategoriacondicion
end type
end forward

global type w_info_planillacategoriacondicion from w_para_informes
string tag = "Consulta Planilla - Control de Calidad de Recepción"
integer x = 14
integer y = 32
integer width = 2126
integer height = 1188
string title = "CONSULTA PLANILLA  DE RECEPCION"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
em_desde em_desde
st_12 st_12
st_13 st_13
em_hasta em_hasta
st_33 st_33
cbx_todosfecha cbx_todosfecha
st_14 st_14
st_2 st_2
uo_selvariedad uo_selvariedad
st_3 st_3
st_8 st_8
gb_3 gb_3
uo_selproductor uo_selproductor
st_44 st_44
uo_selespecies uo_selespecies
st_6 st_6
end type
global w_info_planillacategoriacondicion w_info_planillacategoriacondicion

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	        ii_tipo
String	    		   is_report, is_nula


end variables

on w_info_planillacategoriacondicion.create
int iCurrent
call super::create
this.em_desde=create em_desde
this.st_12=create st_12
this.st_13=create st_13
this.em_hasta=create em_hasta
this.st_33=create st_33
this.cbx_todosfecha=create cbx_todosfecha
this.st_14=create st_14
this.st_2=create st_2
this.uo_selvariedad=create uo_selvariedad
this.st_3=create st_3
this.st_8=create st_8
this.gb_3=create gb_3
this.uo_selproductor=create uo_selproductor
this.st_44=create st_44
this.uo_selespecies=create uo_selespecies
this.st_6=create st_6
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_desde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_hasta
this.Control[iCurrent+5]=this.st_33
this.Control[iCurrent+6]=this.cbx_todosfecha
this.Control[iCurrent+7]=this.st_14
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.uo_selvariedad
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.st_8
this.Control[iCurrent+12]=this.gb_3
this.Control[iCurrent+13]=this.uo_selproductor
this.Control[iCurrent+14]=this.st_44
this.Control[iCurrent+15]=this.uo_selespecies
this.Control[iCurrent+16]=this.st_6
end on

on w_info_planillacategoriacondicion.destroy
call super::destroy
destroy(this.em_desde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_hasta)
destroy(this.st_33)
destroy(this.cbx_todosfecha)
destroy(this.st_14)
destroy(this.st_2)
destroy(this.uo_selvariedad)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.uo_selproductor)
destroy(this.st_44)
destroy(this.uo_selespecies)
destroy(this.st_6)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelEspecies.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelEspecies.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	
	uo_SelEspecies.dw_seleccion.object.codigo[1] = 11
	uo_SelEspecies.Codigo = 11
	uo_SelVariedad.Filtra(11)
	
	em_Desde.Text	= '01/' + String(Today(), 'mm/yyyy')
	em_Hasta.Text	= String(Today())
End If
end event

type st_computador from w_para_informes`st_computador within w_info_planillacategoriacondicion
end type

type st_usuario from w_para_informes`st_usuario within w_info_planillacategoriacondicion
end type

type st_temporada from w_para_informes`st_temporada within w_info_planillacategoriacondicion
end type

type p_logo from w_para_informes`p_logo within w_info_planillacategoriacondicion
end type

type st_titulo from w_para_informes`st_titulo within w_info_planillacategoriacondicion
integer x = 37
integer y = 56
integer width = 1655
string text = "Consulta Planilla de Categorias Condiccón"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_planillacategoriacondicion
string tag = "Imprimir Reporte"
integer x = 1806
integer y = 256
integer taborder = 190
integer weight = 400
fontcharset fontcharset = ansi!
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;Integer	li_fila
Date		ld_Desde, ld_Hasta

SetPointer(HourGlass!)

If cbx_todosfecha.Checked Then
	ld_Desde	=	Date('2000-01-01')
	ld_Hasta	=	Today()
	em_Desde.text = String(ld_Desde, "dd/mm/yyyy")
	em_Hasta.Text = String(ld_Hasta, "dd/mm/yyyy")
Else
	ld_Desde = Date(em_Desde.Text)
	ld_Hasta = Date(em_Hasta.Text)
End If

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_categoriacondicion"
vinf.dw_1.SetTransObject(sqlca)	

li_fila = vinf.dw_1.Retrieve(gi_CodExport, uo_SelEspecies.Codigo, -1, ld_Desde, ld_Hasta, uo_SelProductor.Codigo, uo_SelVariedad.Codigo)
													
If li_fila = -1 Then
	MessageBox( "Error...", "Se ha producido un error en Base de datos :~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "Atencion...", "No existe información para este informe.", Information!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_planillacategoriacondicion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1810
integer y = 536
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_desde from editmask within w_info_planillacategoriacondicion
integer x = 521
integer y = 864
integer width = 338
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_12 from statictext within w_info_planillacategoriacondicion
integer x = 293
integer y = 876
integer width = 215
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_planillacategoriacondicion
integer x = 882
integer y = 876
integer width = 178
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_planillacategoriacondicion
integer x = 1051
integer y = 864
integer width = 338
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_33 from statictext within w_info_planillacategoriacondicion
integer x = 73
integer y = 564
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_info_planillacategoriacondicion
integer x = 1454
integer y = 876
integer width = 91
integer height = 64
integer taborder = 100
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = " "
boolean checked = true
end type

event clicked;If This.Checked Then
	em_Desde.Enabled	=	False
	em_Hasta.Enabled	=	False
Else
	em_Desde.Enabled	=	True
	em_Hasta.Enabled	=	True
	em_Desde.SetFocus()
End If
end event

type st_14 from statictext within w_info_planillacategoriacondicion
integer x = 73
integer y = 336
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_planillacategoriacondicion
integer x = 1399
integer y = 232
integer width = 197
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Todos"
boolean focusrectangle = false
end type

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_planillacategoriacondicion
integer x = 549
integer y = 428
integer width = 1015
integer height = 112
integer taborder = 160
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_info_planillacategoriacondicion
integer x = 73
integer y = 452
integer width = 370
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Variedad"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_planillacategoriacondicion
boolean visible = false
integer x = 1650
integer y = 232
integer width = 78
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Cons."
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_planillacategoriacondicion
integer x = 78
integer y = 780
integer width = 1541
integer height = 228
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 12632256
string text = " Fecha Recepción "
end type

type uo_selproductor from uo_seleccion_productor_mod within w_info_planillacategoriacondicion
event destroy ( )
integer x = 549
integer y = 548
integer width = 1015
integer height = 112
integer taborder = 140
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

type st_44 from statictext within w_info_planillacategoriacondicion
integer x = 37
integer y = 732
integer width = 1655
integer height = 324
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecies from uo_seleccion_especie within w_info_planillacategoriacondicion
integer x = 549
integer y = 336
integer height = 84
integer taborder = 200
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return 

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		
End Choose
end event

on uo_selespecies.destroy
call uo_seleccion_especie::destroy
end on

type st_6 from statictext within w_info_planillacategoriacondicion
integer x = 37
integer y = 184
integer width = 1655
integer height = 544
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

