$PBExportHeader$w_info_resumenreclamos.srw
$PBExportComments$Ventana de consulta planilla de madurez
forward
global type w_info_resumenreclamos from w_para_informes
end type
type gb_3 from groupbox within w_info_resumenreclamos
end type
type st_33 from statictext within w_info_resumenreclamos
end type
type st_14 from statictext within w_info_resumenreclamos
end type
type st_44 from statictext within w_info_resumenreclamos
end type
type st_1 from statictext within w_info_resumenreclamos
end type
type st_2 from statictext within w_info_resumenreclamos
end type
type st_3 from statictext within w_info_resumenreclamos
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_resumenreclamos
end type
type uo_selzonas from uo_seleccion_zonas_mod within w_info_resumenreclamos
end type
type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_resumenreclamos
end type
type st_4 from statictext within w_info_resumenreclamos
end type
type st_5 from statictext within w_info_resumenreclamos
end type
type st_6 from statictext within w_info_resumenreclamos
end type
type st_7 from statictext within w_info_resumenreclamos
end type
type uo_selnaves from uo_seleccion_naves_mod within w_info_resumenreclamos
end type
type uo_seltipo from uo_seleccion_tipotransporte_mod within w_info_resumenreclamos
end type
type cbx_semana from checkbox within w_info_resumenreclamos
end type
type cbx_planilla from checkbox within w_info_resumenreclamos
end type
type em_semana from editmask within w_info_resumenreclamos
end type
type em_planilla from editmask within w_info_resumenreclamos
end type
type cbx_consema from checkbox within w_info_resumenreclamos
end type
type cbx_conspla from checkbox within w_info_resumenreclamos
end type
type st_8 from statictext within w_info_resumenreclamos
end type
type uo_selrecibidor from uo_seleccion_recibidor_mod within w_info_resumenreclamos
end type
type uo_selespecie from uo_seleccion_especie_mod within w_info_resumenreclamos
end type
type ddlb_zonal from dropdownlistbox within w_info_resumenreclamos
end type
type ddlb_impreso from dropdownlistbox within w_info_resumenreclamos
end type
type st_9 from statictext within w_info_resumenreclamos
end type
type st_10 from statictext within w_info_resumenreclamos
end type
type rb_semana from radiobutton within w_info_resumenreclamos
end type
type rb_recibidor from radiobutton within w_info_resumenreclamos
end type
type rb_reclamo from radiobutton within w_info_resumenreclamos
end type
type dw_1 from uo_dw within w_info_resumenreclamos
end type
end forward

global type w_info_resumenreclamos from w_para_informes
integer x = 14
integer y = 32
integer width = 2368
integer height = 2392
string icon = "AppIcon!"
gb_3 gb_3
st_33 st_33
st_14 st_14
st_44 st_44
st_1 st_1
st_2 st_2
st_3 st_3
uo_selvariedad uo_selvariedad
uo_selzonas uo_selzonas
uo_selproductor uo_selproductor
st_4 st_4
st_5 st_5
st_6 st_6
st_7 st_7
uo_selnaves uo_selnaves
uo_seltipo uo_seltipo
cbx_semana cbx_semana
cbx_planilla cbx_planilla
em_semana em_semana
em_planilla em_planilla
cbx_consema cbx_consema
cbx_conspla cbx_conspla
st_8 st_8
uo_selrecibidor uo_selrecibidor
uo_selespecie uo_selespecie
ddlb_zonal ddlb_zonal
ddlb_impreso ddlb_impreso
st_9 st_9
st_10 st_10
rb_semana rb_semana
rb_recibidor rb_recibidor
rb_reclamo rb_reclamo
dw_1 dw_1
end type
global w_info_resumenreclamos w_info_resumenreclamos

type variables
Integer	ii_Zonal, ii_Informe
end variables

on w_info_resumenreclamos.create
int iCurrent
call super::create
this.gb_3=create gb_3
this.st_33=create st_33
this.st_14=create st_14
this.st_44=create st_44
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.uo_selvariedad=create uo_selvariedad
this.uo_selzonas=create uo_selzonas
this.uo_selproductor=create uo_selproductor
this.st_4=create st_4
this.st_5=create st_5
this.st_6=create st_6
this.st_7=create st_7
this.uo_selnaves=create uo_selnaves
this.uo_seltipo=create uo_seltipo
this.cbx_semana=create cbx_semana
this.cbx_planilla=create cbx_planilla
this.em_semana=create em_semana
this.em_planilla=create em_planilla
this.cbx_consema=create cbx_consema
this.cbx_conspla=create cbx_conspla
this.st_8=create st_8
this.uo_selrecibidor=create uo_selrecibidor
this.uo_selespecie=create uo_selespecie
this.ddlb_zonal=create ddlb_zonal
this.ddlb_impreso=create ddlb_impreso
this.st_9=create st_9
this.st_10=create st_10
this.rb_semana=create rb_semana
this.rb_recibidor=create rb_recibidor
this.rb_reclamo=create rb_reclamo
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_3
this.Control[iCurrent+2]=this.st_33
this.Control[iCurrent+3]=this.st_14
this.Control[iCurrent+4]=this.st_44
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.uo_selvariedad
this.Control[iCurrent+9]=this.uo_selzonas
this.Control[iCurrent+10]=this.uo_selproductor
this.Control[iCurrent+11]=this.st_4
this.Control[iCurrent+12]=this.st_5
this.Control[iCurrent+13]=this.st_6
this.Control[iCurrent+14]=this.st_7
this.Control[iCurrent+15]=this.uo_selnaves
this.Control[iCurrent+16]=this.uo_seltipo
this.Control[iCurrent+17]=this.cbx_semana
this.Control[iCurrent+18]=this.cbx_planilla
this.Control[iCurrent+19]=this.em_semana
this.Control[iCurrent+20]=this.em_planilla
this.Control[iCurrent+21]=this.cbx_consema
this.Control[iCurrent+22]=this.cbx_conspla
this.Control[iCurrent+23]=this.st_8
this.Control[iCurrent+24]=this.uo_selrecibidor
this.Control[iCurrent+25]=this.uo_selespecie
this.Control[iCurrent+26]=this.ddlb_zonal
this.Control[iCurrent+27]=this.ddlb_impreso
this.Control[iCurrent+28]=this.st_9
this.Control[iCurrent+29]=this.st_10
this.Control[iCurrent+30]=this.rb_semana
this.Control[iCurrent+31]=this.rb_recibidor
this.Control[iCurrent+32]=this.rb_reclamo
this.Control[iCurrent+33]=this.dw_1
end on

on w_info_resumenreclamos.destroy
call super::destroy
destroy(this.gb_3)
destroy(this.st_33)
destroy(this.st_14)
destroy(this.st_44)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.uo_selvariedad)
destroy(this.uo_selzonas)
destroy(this.uo_selproductor)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.uo_selnaves)
destroy(this.uo_seltipo)
destroy(this.cbx_semana)
destroy(this.cbx_planilla)
destroy(this.em_semana)
destroy(this.em_planilla)
destroy(this.cbx_consema)
destroy(this.cbx_conspla)
destroy(this.st_8)
destroy(this.uo_selrecibidor)
destroy(this.uo_selespecie)
destroy(this.ddlb_zonal)
destroy(this.ddlb_impreso)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.rb_semana)
destroy(this.rb_recibidor)
destroy(this.rb_reclamo)
destroy(this.dw_1)
end on

event open;call super::open;Boolean	lb_Cerrar
String		ls_data

If IsNull(uo_SelProductor.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelEspecie.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelZonas.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelTipo.Codigo)			Then lb_Cerrar	=	True
If IsNull(uo_SelNaves.Codigo) 		Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelProductor.Seleccion(True, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelZonas.Seleccion(True, False)
	uo_SelTipo.Seleccion(True, False)
	uo_SelNaves.Seleccion(True, False)
	uo_SelRecibidor.Seleccion(True, False)
	uo_SelEspecie.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	165
	uo_SelProductor.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	195
	uo_SelVariedad.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	165
	uo_SelZonas.dw_Seleccion.Object.codigo.Dddw.PercentWidth		=	155
	uo_SelTipo.dw_Seleccion.Object.codigo.Dddw.PercentWidth		=	190
	uo_SelNaves.dw_Seleccion.Object.codigo.Dddw.PercentWidth		=	250
	uo_SelRecibidor.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	180
	
	uo_SelEspecie.dw_Seleccion.Object.codigo.Dddw.HScrollBar	= 'No'
	uo_SelProductor.dw_Seleccion.Object.codigo.Dddw.HScrollBar	= 'No'
	uo_SelVariedad.dw_Seleccion.Object.codigo.Dddw.HScrollBar	= 'No'
	uo_SelZonas.dw_Seleccion.Object.codigo.Dddw.HScrollBar		= 'No'
	uo_SelTipo.dw_Seleccion.Object.codigo.Dddw.HScrollBar		= 'No'
	uo_SelNaves.dw_Seleccion.Object.codigo.Dddw.HScrollBar		= 'No'
	uo_SelRecibidor.dw_Seleccion.Object.codigo.Dddw.HScrollBar	= 'No'
		
	uo_SelEspecie.Codigo = Integer(Message.StringParm)
	uo_SelEspecie.dw_Seleccion.Object.Codigo[1] = Integer(Message.StringParm)
	uo_SelEspecie.iuo_Especie.Existe(Integer(Message.StringParm), True, sqlca)
	uo_SelEspecie.Nombre = uo_SelEspecie.iuo_Especie.Nombre	 
	 
	uo_SelVariedad.Filtra(Integer(Message.StringParm))

	
	ddlb_zonal.SelectItem(4)
	ddlb_impreso.SelectItem(4)
	ii_Zonal		= -1
	ii_Informe	=-1
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_resumenreclamos
end type

type st_computador from w_para_informes`st_computador within w_info_resumenreclamos
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumenreclamos
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumenreclamos
end type

type p_logo from w_para_informes`p_logo within w_info_resumenreclamos
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumenreclamos
integer width = 1641
string text = "Resumen Reclamos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumenreclamos
string tag = "Imprimir Reporte"
integer x = 2021
integer y = 480
integer taborder = 100
end type

event pb_acepta::clicked;Integer	li_fila
Long     	ll_Numero, ll_Semana

istr_info.titulo	= 'RESUMEN PLANILLA RECLAMOS DE ' + Upper(uo_SelEspecie.Nombre)

OpenWithParm(vinf,istr_info)

If cbx_Conspla.Checked Then
	ll_Numero = -9
ElseIf cbx_Planilla.Checked Then
	ll_Numero = -1
Else
	ll_Numero = Long(em_Planilla.Text)
End If

If cbx_Consema.Checked Then
	ll_Semana = -9
ElseIf cbx_Semana.Checked Then
	ll_Semana = -1
Else
	ll_Semana = Long(em_Semana.Text)
End If

If rb_Semana.Checked Then
	vinf.dw_1.DataObject = "dw_info_resumen_reclamos"	
ElseIf rb_Recibidor.Checked Then
	vinf.dw_1.DataObject = "dw_info_resumen_reclamos_reci"	
ElseIf rb_Reclamo.Checked Then
	vinf.dw_1.DataObject = "dw_info_resumen_reclamos_recl"	
End If
//vinf.dw_1.Object.Datawindow.Zoom = 10
vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(gi_CodExport, uo_SelEspecie.Codigo, ll_Numero, uo_SelTipo.Codigo, uo_SelNaves.Codigo,&
					ll_Semana, uo_SelRecibidor.Codigo, uo_SelVariedad.Codigo, uo_SelZonas.Codigo, uo_SelProductor.Codigo, ii_Zonal, ii_Informe)
						  
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy('DataWindow.Zoom = 66')
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END If
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumenreclamos
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2021
integer y = 764
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_3 from groupbox within w_info_resumenreclamos
integer x = 306
integer y = 1980
integer width = 1531
integer height = 216
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Tipo de Informe "
end type

type st_33 from statictext within w_info_resumenreclamos
integer x = 288
integer y = 1424
integer width = 471
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
boolean enabled = false
string text = "Semana Liq."
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_resumenreclamos
integer x = 288
integer y = 748
integer width = 370
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
boolean enabled = false
string text = "Zona"
boolean focusrectangle = false
end type

type st_44 from statictext within w_info_resumenreclamos
integer x = 247
integer y = 404
integer width = 1641
integer height = 1840
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_resumenreclamos
integer x = 288
integer y = 484
integer width = 370
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
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_resumenreclamos
integer x = 288
integer y = 616
integer width = 370
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
boolean enabled = false
string text = "Variedad"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_resumenreclamos
integer x = 288
integer y = 880
integer width = 370
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
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_resumenreclamos
integer x = 782
integer y = 592
integer width = 1019
integer taborder = 20
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selzonas from uo_seleccion_zonas_mod within w_info_resumenreclamos
integer x = 782
integer y = 724
integer width = 1019
integer taborder = 30
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelProductor.Filtra(This.Codigo)
		
End Choose
end event

type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_resumenreclamos
integer x = 782
integer y = 856
integer width = 1019
integer taborder = 40
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type st_4 from statictext within w_info_resumenreclamos
integer x = 1646
integer y = 412
integer width = 192
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
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_resumenreclamos
integer x = 288
integer y = 1016
integer width = 471
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
boolean enabled = false
string text = "Recibidor"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_resumenreclamos
integer x = 288
integer y = 1696
integer width = 471
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
boolean enabled = false
string text = "Info. Zonal"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_resumenreclamos
integer x = 288
integer y = 1288
integer width = 471
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
boolean enabled = false
string text = "Nave"
boolean focusrectangle = false
end type

type uo_selnaves from uo_seleccion_naves_mod within w_info_resumenreclamos
integer x = 782
integer y = 1264
integer width = 1019
integer taborder = 70
boolean bringtotop = true
end type

on uo_selnaves.destroy
call uo_seleccion_naves_mod::destroy
end on

type uo_seltipo from uo_seleccion_tipotransporte_mod within w_info_resumenreclamos
integer x = 782
integer y = 1132
integer width = 1019
integer taborder = 60
boolean bringtotop = true
end type

on uo_seltipo.destroy
call uo_seleccion_tipotransporte_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case '*', '**'
		
	Case Else
		uo_SelNaves.Filtra(This.Codigo)
		
End Choose
end event

type cbx_semana from checkbox within w_info_resumenreclamos
integer x = 1701
integer y = 1424
integer width = 82
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
boolean checked = true
end type

event clicked;If This.Checked Then
	em_semana.Text = ''
	em_semana.Enabled = False
Else
	em_semana.Text = ''
	em_semana.Enabled = True
	em_semana.SetFocus()
End If
end event

type cbx_planilla from checkbox within w_info_resumenreclamos
integer x = 1701
integer y = 1560
integer width = 82
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
boolean checked = true
end type

event clicked;If This.Checked Then
	em_planilla.Text = ''
	em_planilla.Enabled = False
Else
	em_planilla.Text = ''
	em_planilla.Enabled = True
	em_planilla.SetFocus()
End If
end event

type em_semana from editmask within w_info_resumenreclamos
integer x = 782
integer y = 1420
integer width = 402
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
string mask = "######"
end type

type em_planilla from editmask within w_info_resumenreclamos
integer x = 782
integer y = 1556
integer width = 402
integer height = 92
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type cbx_consema from checkbox within w_info_resumenreclamos
boolean visible = false
integer x = 1902
integer y = 1424
integer width = 82
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
end type

type cbx_conspla from checkbox within w_info_resumenreclamos
boolean visible = false
integer x = 1897
integer y = 1560
integer width = 82
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
end type

type st_8 from statictext within w_info_resumenreclamos
integer x = 288
integer y = 1152
integer width = 471
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
boolean enabled = false
string text = "Tipo Transporte"
boolean focusrectangle = false
end type

type uo_selrecibidor from uo_seleccion_recibidor_mod within w_info_resumenreclamos
integer x = 782
integer y = 988
integer width = 1019
integer taborder = 50
boolean bringtotop = true
end type

on uo_selrecibidor.destroy
call uo_seleccion_recibidor_mod::destroy
end on

type uo_selespecie from uo_seleccion_especie_mod within w_info_resumenreclamos
integer x = 782
integer y = 460
integer width = 901
integer taborder = 110
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_Selvariedad.Filtra(This.Codigo)
		
End Choose
end event

type ddlb_zonal from dropdownlistbox within w_info_resumenreclamos
integer x = 782
integer y = 1696
integer width = 480
integer height = 400
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean sorted = false
string item[] = {"Si","No","Nulo","Todos"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;Choose Case Index 
	Case 4
		ii_Zonal = -1
		
	Case 2
		ii_Zonal = 0
		
	Case Else
		ii_Zonal = Index
		
End Choose
		
end event

type ddlb_impreso from dropdownlistbox within w_info_resumenreclamos
integer x = 782
integer y = 1836
integer width = 480
integer height = 400
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean sorted = false
string item[] = {"Si","No","Nulo","Todos"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;Choose Case Index 
	Case 4
		ii_Informe = -1
		
	Case 2
		ii_Informe = 0
		
	Case Else
		ii_Informe = Index
		
End Choose
		
end event

type st_9 from statictext within w_info_resumenreclamos
integer x = 288
integer y = 1832
integer width = 471
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
boolean enabled = false
string text = "Impreso"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_resumenreclamos
integer x = 288
integer y = 1560
integer width = 471
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
boolean enabled = false
string text = "Nro.Reclamo"
boolean focusrectangle = false
end type

type rb_semana from radiobutton within w_info_resumenreclamos
integer x = 357
integer y = 2072
integer width = 485
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
string text = "Por Semana"
boolean checked = true
end type

type rb_recibidor from radiobutton within w_info_resumenreclamos
integer x = 850
integer y = 2072
integer width = 507
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
string text = "Por Recibidor"
end type

type rb_reclamo from radiobutton within w_info_resumenreclamos
integer x = 1367
integer y = 2072
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
string text = "Por Reclamo"
end type

type dw_1 from uo_dw within w_info_resumenreclamos
boolean visible = false
integer x = 2048
integer y = 1632
integer width = 137
integer height = 112
integer taborder = 11
boolean bringtotop = true
end type

event clicked;Integer	li_fila
Long     	ll_Numero, ll_Semana
String		ls_Ruta, ls_Archivo, ls_FecGen

SetPointer(HourGlass!)

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

If cbx_Conspla.Checked Then
	ll_Numero = -9
ElseIf cbx_Planilla.Checked Then
	ll_Numero = -1
Else
	ll_Numero = Long(em_Planilla.Text)
End If

If cbx_Consema.Checked Then
	ll_Semana = -9
ElseIf cbx_Semana.Checked Then
	ll_Semana = -1
Else
	ll_Semana = Long(em_Semana.Text)
End If

If rb_Semana.Checked Then
	dw_1.DataObject = "dw_info_resumen_reclamos_excel"
End IF
ls_FecGen = mid(string(today()),1,2)+mid(string(today()),4,2)+mid(string(today()),7,4)
ls_Archivo = '\Reclamos_' + uo_SelEspecie.Nombre + '_' +ls_FecGen +'.xls'

dw_1.SetTransObject(sqlca)

li_fila = dw_1.Retrieve(gi_CodExport, uo_SelEspecie.Codigo, ll_Numero, uo_SelTipo.Codigo, uo_SelNaves.Codigo,&
					ll_Semana, uo_SelRecibidor.Codigo, uo_SelVariedad.Codigo, uo_SelZonas.Codigo, uo_SelProductor.Codigo,&
					ii_Zonal, ii_Informe)
						  
If li_fila = -1 Then
	MessageBox("Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox("No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If dw_1.SaveAs(ls_Ruta + ls_Archivo, Excel8!, True) = 1 Then
		MessageBox('Atencion', 'Se genero archivo ' + ls_Archivo)
	Else
		MessageBox('Atencion', 'No se pudo generar archivo ' + ls_Archivo)		
	End If	
End If

SetPointer(Arrow!)
end event

