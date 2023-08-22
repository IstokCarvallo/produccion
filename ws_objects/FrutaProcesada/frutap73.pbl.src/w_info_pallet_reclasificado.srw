$PBExportHeader$w_info_pallet_reclasificado.srw
forward
global type w_info_pallet_reclasificado from w_para_informes
end type
type st_1 from statictext within w_info_pallet_reclasificado
end type
type st_2 from statictext within w_info_pallet_reclasificado
end type
type em_desde from editmask within w_info_pallet_reclasificado
end type
type dw_cliente from datawindow within w_info_pallet_reclasificado
end type
type st_6 from statictext within w_info_pallet_reclasificado
end type
type dw_planta from datawindow within w_info_pallet_reclasificado
end type
type cbx_planta from checkbox within w_info_pallet_reclasificado
end type
type st_7 from statictext within w_info_pallet_reclasificado
end type
type em_hasta from editmask within w_info_pallet_reclasificado
end type
type dw_pesoneto from datawindow within w_info_pallet_reclasificado
end type
type tit_peso from statictext within w_info_pallet_reclasificado
end type
type st_11 from statictext within w_info_pallet_reclasificado
end type
type gb_3 from groupbox within w_info_pallet_reclasificado
end type
type cbx_1 from checkbox within w_info_pallet_reclasificado
end type
type st_4 from statictext within w_info_pallet_reclasificado
end type
end forward

global type w_info_pallet_reclasificado from w_para_informes
integer x = 14
integer y = 32
integer width = 2679
integer height = 1248
string title = "Pallets Reclasificados"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Productiva\FrutaProcesada\FrutaPro.ico"
st_1 st_1
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
cbx_planta cbx_planta
st_7 st_7
em_hasta em_hasta
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_11 st_11
gb_3 gb_3
cbx_1 cbx_1
st_4 st_4
end type
global w_info_pallet_reclasificado w_info_pallet_reclasificado

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_especie, idwc_productor, idwc_packing, idwc_pesoneto

String is_NomPlanta
end variables

on w_info_pallet_reclasificado.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.cbx_planta=create cbx_planta
this.st_7=create st_7
this.em_hasta=create em_hasta
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_11=create st_11
this.gb_3=create gb_3
this.cbx_1=create cbx_1
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.dw_cliente
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.dw_planta
this.Control[iCurrent+7]=this.cbx_planta
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.em_hasta
this.Control[iCurrent+10]=this.dw_pesoneto
this.Control[iCurrent+11]=this.tit_peso
this.Control[iCurrent+12]=this.st_11
this.Control[iCurrent+13]=this.gb_3
this.Control[iCurrent+14]=this.cbx_1
this.Control[iCurrent+15]=this.st_4
end on

on w_info_pallet_reclasificado.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.cbx_planta)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_11)
destroy(this.gb_3)
destroy(this.cbx_1)
destroy(this.st_4)
end on

event open;x	=	0
y	=	0

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve(gi_CodExport)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)


em_desde.Text				=	String(Today())//String(RelativeDate(Today(), -366))
em_hasta.Text				=	String(Today())
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	= 	"0"							//	planta
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final



end event

type st_computador from w_para_informes`st_computador within w_info_pallet_reclasificado
end type

type st_usuario from w_para_informes`st_usuario within w_info_pallet_reclasificado
end type

type st_temporada from w_para_informes`st_temporada within w_info_pallet_reclasificado
end type

type p_logo from w_para_informes`p_logo within w_info_pallet_reclasificado
end type

type st_titulo from w_para_informes`st_titulo within w_info_pallet_reclasificado
integer width = 1847
string text = "Listado de Pallets Reclasificados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_pallet_reclasificado
string tag = "Imprimir Reporte"
integer x = 2222
integer y = 516
integer taborder = 170
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha

istr_info.titulo	= 'LISTADO DE PALLETS RECLASIFICADOS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_pallet_reclasificado"

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])
ld_desde			=	Date(istr_mant.argumento[3])
ld_hasta			=	Date(istr_mant.argumento[4])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta


vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, li_planta, ld_desde, ld_hasta)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
//		vinf.dw_1.Object.titulo_informe.text = 'Pallets Historicos'
//		vinf.dw_1.Modify("guia.text = '" + ls_nroguia + "'")
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
//		vinf.dw_1.Modify("Cajas.text = '" + ls_cajas + "'")
		IF gs_Ambiente <> 'Windows' THEN
			F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		END IF
	END IF	
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_pallet_reclasificado
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2222
integer y = 804
integer taborder = 180
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_pallet_reclasificado
integer x = 338
integer y = 656
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_pallet_reclasificado
integer x = 338
integer y = 820
integer width = 425
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_pallet_reclasificado
integer x = 782
integer y = 804
integer width = 393
integer height = 96
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[3]	=	This.Text
end event

type dw_cliente from datawindow within w_info_pallet_reclasificado
integer x = 782
integer y = 476
integer width = 1143
integer height = 104
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null

SetNull(ls_null)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	idwc_planta.Retrieve(1)
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_pallet_reclasificado
integer x = 338
integer y = 476
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_pallet_reclasificado
integer x = 782
integer y = 656
integer width = 969
integer height = 92
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null
Integer	li_Cliente

SetNull(ls_null)

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_planta from checkbox within w_info_pallet_reclasificado
integer x = 782
integer y = 580
integer width = 402
integer height = 76
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_planta.Enabled			=	False
	istr_mant.argumento[2]	=	'0'
	dw_planta.reset()
	dw_planta.insertrow(0)
ELSE
	dw_planta.Enabled			=	True
	dw_planta.SetFocus()
END IF
end event

type st_7 from statictext within w_info_pallet_reclasificado
integer x = 1216
integer y = 820
integer width = 297
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_pallet_reclasificado
integer x = 1550
integer y = 804
integer width = 393
integer height = 96
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[4]	=	This.Text
end event

type dw_pesoneto from datawindow within w_info_pallet_reclasificado
boolean visible = false
integer x = 759
integer y = 1728
integer width = 544
integer height = 84
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_pallet_reclasificado
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

type st_11 from statictext within w_info_pallet_reclasificado
integer x = 242
integer y = 768
integer width = 1847
integer height = 176
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

type gb_3 from groupbox within w_info_pallet_reclasificado
boolean visible = false
integer x = 174
integer y = 1412
integer width = 1614
integer height = 280
integer taborder = 190
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type cbx_1 from checkbox within w_info_pallet_reclasificado
boolean visible = false
integer x = 617
integer y = 1468
integer width = 631
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cajas Equivalentes"
end type

event clicked;IF This.Checked THEN
	tit_peso.Enabled		=	True
	dw_pesoneto.Enabled	=	True
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(255,255,255)))

ELSE
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))
END IF

end event

type st_4 from statictext within w_info_pallet_reclasificado
integer x = 242
integer y = 440
integer width = 1847
integer height = 328
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

