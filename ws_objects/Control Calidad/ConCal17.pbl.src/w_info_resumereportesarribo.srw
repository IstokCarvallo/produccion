$PBExportHeader$w_info_resumereportesarribo.srw
forward
global type w_info_resumereportesarribo from w_para_informes
end type
type st_33 from statictext within w_info_resumereportesarribo
end type
type st_14 from statictext within w_info_resumereportesarribo
end type
type st_1 from statictext within w_info_resumereportesarribo
end type
type st_2 from statictext within w_info_resumereportesarribo
end type
type st_3 from statictext within w_info_resumereportesarribo
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_resumereportesarribo
end type
type uo_selzonas from uo_seleccion_zonas_mod within w_info_resumereportesarribo
end type
type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_resumereportesarribo
end type
type st_4 from statictext within w_info_resumereportesarribo
end type
type st_7 from statictext within w_info_resumereportesarribo
end type
type uo_selnaves from uo_seleccion_naves_mod within w_info_resumereportesarribo
end type
type uo_seltipo from uo_seleccion_tipotransporte_mod within w_info_resumereportesarribo
end type
type cbx_semana from checkbox within w_info_resumereportesarribo
end type
type em_semana from editmask within w_info_resumereportesarribo
end type
type cbx_consema from checkbox within w_info_resumereportesarribo
end type
type st_8 from statictext within w_info_resumereportesarribo
end type
type uo_selespecie from uo_seleccion_especie_mod within w_info_resumereportesarribo
end type
type st_5 from statictext within w_info_resumereportesarribo
end type
type rb_comercial from radiobutton within w_info_resumereportesarribo
end type
type rb_productor from radiobutton within w_info_resumereportesarribo
end type
type gb_3 from groupbox within w_info_resumereportesarribo
end type
type st_44 from statictext within w_info_resumereportesarribo
end type
end forward

global type w_info_resumereportesarribo from w_para_informes
integer x = 14
integer y = 32
integer width = 2267
integer height = 1948
string icon = "AppIcon!"
st_33 st_33
st_14 st_14
st_1 st_1
st_2 st_2
st_3 st_3
uo_selvariedad uo_selvariedad
uo_selzonas uo_selzonas
uo_selproductor uo_selproductor
st_4 st_4
st_7 st_7
uo_selnaves uo_selnaves
uo_seltipo uo_seltipo
cbx_semana cbx_semana
em_semana em_semana
cbx_consema cbx_consema
st_8 st_8
uo_selespecie uo_selespecie
st_5 st_5
rb_comercial rb_comercial
rb_productor rb_productor
gb_3 gb_3
st_44 st_44
end type
global w_info_resumereportesarribo w_info_resumereportesarribo

type variables

end variables

on w_info_resumereportesarribo.create
int iCurrent
call super::create
this.st_33=create st_33
this.st_14=create st_14
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.uo_selvariedad=create uo_selvariedad
this.uo_selzonas=create uo_selzonas
this.uo_selproductor=create uo_selproductor
this.st_4=create st_4
this.st_7=create st_7
this.uo_selnaves=create uo_selnaves
this.uo_seltipo=create uo_seltipo
this.cbx_semana=create cbx_semana
this.em_semana=create em_semana
this.cbx_consema=create cbx_consema
this.st_8=create st_8
this.uo_selespecie=create uo_selespecie
this.st_5=create st_5
this.rb_comercial=create rb_comercial
this.rb_productor=create rb_productor
this.gb_3=create gb_3
this.st_44=create st_44
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_33
this.Control[iCurrent+2]=this.st_14
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.uo_selvariedad
this.Control[iCurrent+7]=this.uo_selzonas
this.Control[iCurrent+8]=this.uo_selproductor
this.Control[iCurrent+9]=this.st_4
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.uo_selnaves
this.Control[iCurrent+12]=this.uo_seltipo
this.Control[iCurrent+13]=this.cbx_semana
this.Control[iCurrent+14]=this.em_semana
this.Control[iCurrent+15]=this.cbx_consema
this.Control[iCurrent+16]=this.st_8
this.Control[iCurrent+17]=this.uo_selespecie
this.Control[iCurrent+18]=this.st_5
this.Control[iCurrent+19]=this.rb_comercial
this.Control[iCurrent+20]=this.rb_productor
this.Control[iCurrent+21]=this.gb_3
this.Control[iCurrent+22]=this.st_44
end on

on w_info_resumereportesarribo.destroy
call super::destroy
destroy(this.st_33)
destroy(this.st_14)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.uo_selvariedad)
destroy(this.uo_selzonas)
destroy(this.uo_selproductor)
destroy(this.st_4)
destroy(this.st_7)
destroy(this.uo_selnaves)
destroy(this.uo_seltipo)
destroy(this.cbx_semana)
destroy(this.em_semana)
destroy(this.cbx_consema)
destroy(this.st_8)
destroy(this.uo_selespecie)
destroy(this.st_5)
destroy(this.rb_comercial)
destroy(this.rb_productor)
destroy(this.gb_3)
destroy(this.st_44)
end on

event open;call super::open;Boolean	lb_Cerrar

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
	
	uo_SelEspecie.Codigo = Integer(Message.StringParm)
	uo_SelEspecie.dw_Seleccion.Object.Codigo[1] = Integer(Message.StringParm)
	uo_SelVariedad.Filtra(Integer(Message.StringParm))
	uo_SelTipo.Codigo = 'M'
	uo_SelTipo.dw_Seleccion.Object.Codigo[1] = 'M'
	uo_SelTipo.cbx_Todos.Checked = False
End If
end event

type st_computador from w_para_informes`st_computador within w_info_resumereportesarribo
integer x = 1230
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumereportesarribo
integer x = 1230
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumereportesarribo
integer x = 1230
end type

type p_logo from w_para_informes`p_logo within w_info_resumereportesarribo
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumereportesarribo
integer width = 1641
string text = "Reportes Arribo"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumereportesarribo
string tag = "Imprimir Reporte"
integer x = 1934
integer y = 572
integer taborder = 100
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;Integer	li_fila
Long     	ll_Semana

SetPointer(HourGlass!)

istr_info.titulo	= 'RESUMEN REPORTES ARRIBO DE ' + Upper(uo_SelEspecie.Nombre)

OpenWithParm(vinf,istr_info)

If cbx_Semana.Checked Then
	ll_Semana	= -1
Else
	If IsNull(em_Semana.Text) Then
		MessageBox('Atención', 'No se ha ingresado Nro. de Semana.')
		Return -1
	Else
		ll_Semana	= Integer(em_Semana.Text)
	End If
End If

If rb_comercial.Checked Then 
	vinf.dw_1.DataObject = "dw_info_resumereportes_comercial"
Else
	vinf.dw_1.DataObject = "dw_info_resumereportes"
End If

vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(gi_CodExport, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, uo_SelZonas.Codigo,&
				uo_SelProductor.Codigo, ll_semana, uo_SelTipo.Codigo, uo_SelNaves.Codigo)
						  
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Zoom = 82')
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumereportesarribo
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1934
integer y = 852
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_33 from statictext within w_info_resumereportesarribo
integer x = 293
integer y = 1300
integer width = 471
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Semana Liq."
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_resumereportesarribo
integer x = 293
integer y = 780
integer width = 370
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
string text = "Zona"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_resumereportesarribo
integer x = 293
integer y = 520
integer width = 370
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_resumereportesarribo
integer x = 293
integer y = 652
integer width = 370
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_resumereportesarribo
integer x = 293
integer y = 908
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_resumereportesarribo
integer x = 786
integer y = 628
integer width = 1019
integer taborder = 20
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selzonas from uo_seleccion_zonas_mod within w_info_resumereportesarribo
integer x = 786
integer y = 756
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

type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_resumereportesarribo
integer x = 786
integer y = 884
integer width = 1019
integer taborder = 40
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

type st_4 from statictext within w_info_resumereportesarribo
integer x = 1650
integer y = 448
integer width = 192
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_resumereportesarribo
integer x = 293
integer y = 1160
integer width = 471
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Nave"
boolean focusrectangle = false
end type

type uo_selnaves from uo_seleccion_naves_mod within w_info_resumereportesarribo
integer x = 786
integer y = 1136
integer width = 1019
integer taborder = 70
boolean bringtotop = true
end type

on uo_selnaves.destroy
call uo_seleccion_naves_mod::destroy
end on

type uo_seltipo from uo_seleccion_tipotransporte_mod within w_info_resumereportesarribo
integer x = 786
integer y = 1012
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

type cbx_semana from checkbox within w_info_resumereportesarribo
integer x = 1701
integer y = 1292
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

type em_semana from editmask within w_info_resumereportesarribo
integer x = 786
integer y = 1296
integer width = 402
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
borderstyle borderstyle = stylelowered!
string mask = "######"
end type

type cbx_consema from checkbox within w_info_resumereportesarribo
boolean visible = false
integer x = 1902
integer y = 1292
integer width = 82
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
end type

type st_8 from statictext within w_info_resumereportesarribo
integer x = 293
integer y = 1032
integer width = 471
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Tipo Transporte"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie_mod within w_info_resumereportesarribo
integer x = 786
integer y = 496
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

type st_5 from statictext within w_info_resumereportesarribo
integer x = 251
integer y = 440
integer width = 1641
integer height = 1012
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_comercial from radiobutton within w_info_resumereportesarribo
integer x = 462
integer y = 1524
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Comercial"
boolean checked = true
end type

type rb_productor from radiobutton within w_info_resumereportesarribo
integer x = 1170
integer y = 1524
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Productor"
end type

type gb_3 from groupbox within w_info_resumereportesarribo
integer x = 306
integer y = 1460
integer width = 1490
integer height = 184
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 33543637
string text = " Tipo de Informe "
end type

type st_44 from statictext within w_info_resumereportesarribo
integer x = 251
integer y = 1456
integer width = 1641
integer height = 232
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

