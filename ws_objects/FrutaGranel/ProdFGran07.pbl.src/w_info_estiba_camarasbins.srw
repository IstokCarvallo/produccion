$PBExportHeader$w_info_estiba_camarasbins.srw
forward
global type w_info_estiba_camarasbins from w_para_informes
end type
type st_1 from statictext within w_info_estiba_camarasbins
end type
type st_6 from statictext within w_info_estiba_camarasbins
end type
type st_3 from statictext within w_info_estiba_camarasbins
end type
type st_8 from statictext within w_info_estiba_camarasbins
end type
type tit_peso from statictext within w_info_estiba_camarasbins
end type
type st_variedad from statictext within w_info_estiba_camarasbins
end type
type st_5 from statictext within w_info_estiba_camarasbins
end type
type st_4 from statictext within w_info_estiba_camarasbins
end type
type dw_1 from datawindow within w_info_estiba_camarasbins
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_estiba_camarasbins
end type
type uo_selplanta from uo_seleccion_plantas within w_info_estiba_camarasbins
end type
type uo_selcamara from uo_seleccion_camarasbode within w_info_estiba_camarasbins
end type
type uo_selespecie from uo_seleccion_especie within w_info_estiba_camarasbins
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_estiba_camarasbins
end type
end forward

global type w_info_estiba_camarasbins from w_para_informes
integer x = 14
integer y = 32
integer width = 2277
integer height = 1500
string title = "Informe Estiba Cámaras"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "\Desarrollo\Productiva\FrutaGranelPackingTerceros\ProdFGranelPP.ico"
st_1 st_1
st_6 st_6
st_3 st_3
st_8 st_8
tit_peso tit_peso
st_variedad st_variedad
st_5 st_5
st_4 st_4
dw_1 dw_1
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
uo_selcamara uo_selcamara
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
end type
global w_info_estiba_camarasbins w_info_estiba_camarasbins

type variables

end variables

on w_info_estiba_camarasbins.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_6=create st_6
this.st_3=create st_3
this.st_8=create st_8
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.st_5=create st_5
this.st_4=create st_4
this.dw_1=create dw_1
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.uo_selcamara=create uo_selcamara
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.tit_peso
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.dw_1
this.Control[iCurrent+10]=this.uo_selcliente
this.Control[iCurrent+11]=this.uo_selplanta
this.Control[iCurrent+12]=this.uo_selcamara
this.Control[iCurrent+13]=this.uo_selespecie
this.Control[iCurrent+14]=this.uo_selvariedad
end on

on w_info_estiba_camarasbins.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.dw_1)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.uo_selcamara)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
end on

event open;Boolean lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCamara.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelVariedad.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCamara.Seleccion(True, False)
	uo_SelEspecie.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	
	uo_SelPlanta.Filtra(1)
	uo_SelCamara.Filtra(uo_SelPlanta.Codigo)
End If
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_estiba_camarasbins
integer x = 2245
integer y = 592
end type

type st_computador from w_para_informes`st_computador within w_info_estiba_camarasbins
integer x = 1024
integer width = 1467
end type

type st_usuario from w_para_informes`st_usuario within w_info_estiba_camarasbins
integer x = 1024
integer width = 1467
end type

type st_temporada from w_para_informes`st_temporada within w_info_estiba_camarasbins
integer x = 1024
integer width = 1467
end type

type p_logo from w_para_informes`p_logo within w_info_estiba_camarasbins
end type

type st_titulo from w_para_informes`st_titulo within w_info_estiba_camarasbins
integer width = 1481
string text = "Informe Estiba Cámaras"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_estiba_camarasbins
string tag = "Imprimir Reporte"
integer x = 1911
integer y = 564
integer taborder = 100
end type

event pb_acepta::clicked;Integer	fila

SetPointer(HourGlass!)

OpenWithParm(vinf, istr_info)
istr_info.titulo	= 'Informe Plano de Cámaras Por Bins'
vinf.dw_1.DataObject = "dw_info_estiba_camarasbins"
vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelCamara.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("cliente.text = 'Cliente "+uo_SelCliente.Nombre+ "'")
	vinf.dw_1.Modify("planta.text = 'Planta "+uo_SelPlanta.Nombre+ "'")
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_estiba_camarasbins
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1911
integer y = 824
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_estiba_camarasbins
integer x = 302
integer y = 588
integer width = 288
integer height = 76
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_estiba_camarasbins
integer x = 302
integer y = 484
integer width = 288
integer height = 60
boolean bringtotop = true
integer textsize = -10
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

type st_3 from statictext within w_info_estiba_camarasbins
integer x = 297
integer y = 1016
integer width = 288
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
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_estiba_camarasbins
integer x = 302
integer y = 784
integer width = 288
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
string text = "Cámara"
boolean focusrectangle = false
end type

type tit_peso from statictext within w_info_estiba_camarasbins
boolean visible = false
integer x = 567
integer y = 1744
integer width = 183
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_estiba_camarasbins
integer x = 297
integer y = 1192
integer width = 288
integer height = 96
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

type st_5 from statictext within w_info_estiba_camarasbins
integer x = 242
integer y = 908
integer width = 1490
integer height = 444
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_estiba_camarasbins
integer x = 242
integer y = 420
integer width = 1490
integer height = 484
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_estiba_camarasbins
boolean visible = false
integer x = 1902
integer y = 312
integer width = 151
integer height = 132
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_estiba_camarasbins"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_estiba_camarasbins
event destroy ( )
integer x = 631
integer y = 472
integer height = 92
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_estiba_camarasbins
event destroy ( )
integer x = 631
integer y = 580
integer height = 92
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelCamara.Filtra(-1)
		
	Case Else
		uo_SelCamara.Filtra(This.Codigo)
		
End Choose
end event

type uo_selcamara from uo_seleccion_camarasbode within w_info_estiba_camarasbins
integer x = 631
integer y = 700
integer taborder = 120
boolean bringtotop = true
end type

on uo_selcamara.destroy
call uo_seleccion_camarasbode::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_estiba_camarasbins
event destroy ( )
integer x = 631
integer y = 916
integer taborder = 120
boolean bringtotop = true
end type

on uo_selespecie.destroy
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

type uo_selvariedad from uo_seleccion_variedad within w_info_estiba_camarasbins
integer x = 631
integer y = 1100
integer taborder = 130
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

