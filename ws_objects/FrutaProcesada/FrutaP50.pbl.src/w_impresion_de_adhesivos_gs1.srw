$PBExportHeader$w_impresion_de_adhesivos_gs1.srw
$PBExportComments$Emisión Solicitud de Inspección.
forward
global type w_impresion_de_adhesivos_gs1 from window
end type
type sle_movto from singlelineedit within w_impresion_de_adhesivos_gs1
end type
type st_1 from statictext within w_impresion_de_adhesivos_gs1
end type
type dw_2 from datawindow within w_impresion_de_adhesivos_gs1
end type
type cb_2 from commandbutton within w_impresion_de_adhesivos_gs1
end type
type cb_1 from commandbutton within w_impresion_de_adhesivos_gs1
end type
type pb_lectura from picturebutton within w_impresion_de_adhesivos_gs1
end type
type iuo_especie from uo_seleccion_especie within w_impresion_de_adhesivos_gs1
end type
type iuo_cliente from uo_seleccion_clientesprod within w_impresion_de_adhesivos_gs1
end type
type iuo_planta from uo_seleccion_plantas within w_impresion_de_adhesivos_gs1
end type
type dw_1 from datawindow within w_impresion_de_adhesivos_gs1
end type
type st_5 from statictext within w_impresion_de_adhesivos_gs1
end type
type st_4 from statictext within w_impresion_de_adhesivos_gs1
end type
type st_3 from statictext within w_impresion_de_adhesivos_gs1
end type
type st_2 from statictext within w_impresion_de_adhesivos_gs1
end type
type st_encabe from statictext within w_impresion_de_adhesivos_gs1
end type
type pb_nuevo from picturebutton within w_impresion_de_adhesivos_gs1
end type
type pb_salir from picturebutton within w_impresion_de_adhesivos_gs1
end type
type pb_imprimir from picturebutton within w_impresion_de_adhesivos_gs1
end type
type gb_3 from groupbox within w_impresion_de_adhesivos_gs1
end type
type gb_2 from groupbox within w_impresion_de_adhesivos_gs1
end type
type gb_1 from groupbox within w_impresion_de_adhesivos_gs1
end type
end forward

global type w_impresion_de_adhesivos_gs1 from window
integer width = 3497
integer height = 2072
boolean titlebar = true
string title = "EMISION DE ADHESIVOS PARA VENTANA DE PALLETS"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 553648127
string icon = "AppIcon!"
boolean center = true
event ue_nuevo ( )
event ue_recuperadatos ( )
event ue_imprimir ( )
sle_movto sle_movto
st_1 st_1
dw_2 dw_2
cb_2 cb_2
cb_1 cb_1
pb_lectura pb_lectura
iuo_especie iuo_especie
iuo_cliente iuo_cliente
iuo_planta iuo_planta
dw_1 dw_1
st_5 st_5
st_4 st_4
st_3 st_3
st_2 st_2
st_encabe st_encabe
pb_nuevo pb_nuevo
pb_salir pb_salir
pb_imprimir pb_imprimir
gb_3 gb_3
gb_2 gb_2
gb_1 gb_1
end type
global w_impresion_de_adhesivos_gs1 w_impresion_de_adhesivos_gs1

type variables
Integer		ii_procedencia
end variables

forward prototypes
public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia)
end prototypes

event ue_nuevo();iuo_cliente.seleccion(False, False)
iuo_planta.seleccion(False, False)

//iuo_cliente.Bloquear(False)
//iuo_planta.Bloquear(False)

iuo_cliente.Codigo										=	gi_CodExport
iuo_cliente.dw_seleccion.Object.codigo[1]			=	iuo_cliente.Codigo

iuo_planta.Codigo											=	gi_CodPlanta
iuo_planta.dw_seleccion.Object.codigo[1]			=	iuo_planta.Codigo

iuo_especie.seleccion(True, False)
iuo_especie.Todos(True)
iuo_especie.Enabled	=	True

dw_1.Reset()

cb_1.Enabled			=	False
cb_2.Enabled			=	False
pb_imprimir.Enabled	=	False

end event

event ue_recuperadatos();Integer	respuesta
Long		ll_movto


ll_movto	=	Long(sle_movto.Text)

DO
	IF dw_1.Retrieve(iuo_planta.codigo, &
						  iuo_cliente.codigo, &
						  ll_movto) = -1 THEN

		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		IF dw_1.RowCount() < 1 THEN
			Messagebox("Advertencia", "No existen pallets para los criterios ingresados")
			PostEvent("ue_nuevo")
		ELSE
			pb_imprimir.Enabled	=	True
			cb_1.Enabled			=	True
			cb_2.Enabled			=	True
			
//			iuo_cliente.Bloquear(True)
//			iuo_planta.Bloquear(True)
			iuo_especie.Enabled	=	False
			
		END IF
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir();Integer	li_fila, li_codigo, li_pagina, li_nueva
String	ls_codigo

FOR li_fila = 1 TO dw_1.RowCount()
	IF dw_1.IsSelected(li_fila) THEN
		ls_codigo	=	cargacodigo(iuo_cliente.codigo, iuo_planta.codigo, &
											dw_1.Object.paen_numero[li_fila], ii_procedencia)
											
		FOR li_pagina = 1 TO 2
			li_nueva											=	dw_2.InsertRow(0)
			dw_2.Object.ole_codigo1.Object.Text 	=	ls_codigo
			dw_2.Object.ole_codigo2.Object.Text 	=	ls_codigo
			dw_2.Object.ole_codigo3.Object.Text 	=	ls_codigo
			
			IF li_pagina = 2 THEN
				dw_2.Object.ole_codigo3.visible		=	False
			ELSE
				dw_2.Object.ole_codigo3.visible		=	True
			END IF
			
			dw_2.Print()
			dw_2.Reset()
			
		NEXT
	END IF
NEXT
end event

public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia);String ls_respuesta

DECLARE Codigo PROCEDURE FOR dbo.genera_adhesivos_pallets  
        @Planta 		= 	:al_planta,   
        @Cliente 		= 	:ai_cliente,   
        @Pallet 		= 	:al_pallet,   
        @Procedencia = 	:ai_procedencia  
	USING SQLCA;
			
EXECUTE Codigo;

IF SQLCA.SQLCode = -1 THEN
	F_ErrorBaseDatos(SQLCA, "Lectura del Procedimiento Almacenado " + &
									"genera_adhesivos_pallets" )
							
ELSE
	FEtCH Codigo INTO :ls_respuesta;
END IF	
	
CLOSE Codigo;

RETURN ls_respuesta 

end function

on w_impresion_de_adhesivos_gs1.create
this.sle_movto=create sle_movto
this.st_1=create st_1
this.dw_2=create dw_2
this.cb_2=create cb_2
this.cb_1=create cb_1
this.pb_lectura=create pb_lectura
this.iuo_especie=create iuo_especie
this.iuo_cliente=create iuo_cliente
this.iuo_planta=create iuo_planta
this.dw_1=create dw_1
this.st_5=create st_5
this.st_4=create st_4
this.st_3=create st_3
this.st_2=create st_2
this.st_encabe=create st_encabe
this.pb_nuevo=create pb_nuevo
this.pb_salir=create pb_salir
this.pb_imprimir=create pb_imprimir
this.gb_3=create gb_3
this.gb_2=create gb_2
this.gb_1=create gb_1
this.Control[]={this.sle_movto,&
this.st_1,&
this.dw_2,&
this.cb_2,&
this.cb_1,&
this.pb_lectura,&
this.iuo_especie,&
this.iuo_cliente,&
this.iuo_planta,&
this.dw_1,&
this.st_5,&
this.st_4,&
this.st_3,&
this.st_2,&
this.st_encabe,&
this.pb_nuevo,&
this.pb_salir,&
this.pb_imprimir,&
this.gb_3,&
this.gb_2,&
this.gb_1}
end on

on w_impresion_de_adhesivos_gs1.destroy
destroy(this.sle_movto)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.cb_2)
destroy(this.cb_1)
destroy(this.pb_lectura)
destroy(this.iuo_especie)
destroy(this.iuo_cliente)
destroy(this.iuo_planta)
destroy(this.dw_1)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_encabe)
destroy(this.pb_nuevo)
destroy(this.pb_salir)
destroy(this.pb_imprimir)
destroy(this.gb_3)
destroy(this.gb_2)
destroy(this.gb_1)
end on

event open;dw_1.SetTransObject(sqlca)

ii_procedencia				=	Integer(Message.StringParm)

TriggerEVent("ue_nuevo")
end event

event resize;//Integer		li_posi_y, li_objeto
//
//dw_1.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - dw_1.y - 75)
//
//dw_1.x					= 78
//st_2.X					= dw_1.x
//cb_1.x					= dw_1.x + 165
//cb_2.x					= cb_1.x + cb_1.width
//st_2.Resize(dw_1.Width, dw_1.Height + 156)
//
//st_encabe.width		= st_2.width
//st_encabe.x				= st_2.x
//
//dw_1.x 					= dw_1.x + 41
//dw_1.width				= dw_1.width - 82
//dw_1.Height				= dw_1.Height - 41
//
//gb_1.x 					= This.WorkSpaceWidth() - 351
//gb_1.y 					= 33
//gb_1.width				= 275
//gb_1.height				= 180 * 1 + 97 /*  (1 Botón)  */
//
//pb_lectura.x			= This.WorkSpaceWidth() - 292
//pb_lectura.y			= gb_1.y + 88
//pb_lectura.width		= 156
//pb_lectura.height		= 133
//
//gb_2.x 					= This.WorkSpaceWidth() - 351
//gb_2.width				= 275
//
//pb_nuevo.x				= This.WorkSpaceWidth() - 292
//pb_nuevo.width			= 156
//pb_nuevo.height		= 133
//
//pb_imprimir.x			= This.WorkSpaceWidth() - 292
//pb_imprimir.width		= 156
//pb_imprimir.height	= 133
//
//IF st_encabe.Visible THEN
//	gb_2.y 					= dw_1.y - 36
//ELSE
//	gb_2.y 					= gb_1.y + gb_1.height + 23
//END IF
//
//li_posi_y	= gb_2.y - 92
//
//IF pb_nuevo.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_nuevo.y	= li_posi_y
//END IF
//
//IF pb_imprimir.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_imprimir.y	= li_posi_y
//END IF
//
//gb_2.height				= 180 * li_objeto + 97
//gb_3.x 					= This.WorkSpaceWidth() - 351
//gb_3.y 					= This.WorkSpaceHeight() - 345
//gb_3.width				= 275
//gb_3.height				= 180 * 1 + 97 /*  (1 Botón)  */
//
//pb_salir.x				= This.WorkSpaceWidth() - 292
//pb_salir.y				= gb_3.y + 88
//pb_salir.width			= 156
//pb_salir.height		= 133
end event

type sle_movto from singlelineedit within w_impresion_de_adhesivos_gs1
integer x = 1979
integer y = 300
integer width = 635
integer height = 96
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_impresion_de_adhesivos_gs1
integer x = 1623
integer y = 312
integer width = 352
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Pallet"
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_impresion_de_adhesivos_gs1
boolean visible = false
integer x = 50
integer y = 52
integer width = 151
integer height = 112
integer taborder = 110
string title = "none"
string dataobject = "dw_adhesivos_pallets"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type cb_2 from commandbutton within w_impresion_de_adhesivos_gs1
integer x = 608
integer y = 596
integer width = 402
integer height = 100
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Ninguno"
end type

event clicked;dw_1.SelectRow(0, False)
end event

type cb_1 from commandbutton within w_impresion_de_adhesivos_gs1
integer x = 197
integer y = 596
integer width = 402
integer height = 100
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Todos"
end type

event clicked;dw_1.SelectRow(0, True)
end event

type pb_lectura from picturebutton within w_impresion_de_adhesivos_gs1
integer x = 3122
integer y = 304
integer width = 297
integer height = 240
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Busqueda.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Busqueda.png"
alignment htextalign = left!
end type

event clicked;parent.triggerevent("ue_recuperadatos")
end event

type iuo_especie from uo_seleccion_especie within w_impresion_de_adhesivos_gs1
boolean visible = false
integer x = 1975
integer y = 84
integer height = 168
integer taborder = 30
end type

on iuo_especie.destroy
call uo_seleccion_especie::destroy
end on

type iuo_cliente from uo_seleccion_clientesprod within w_impresion_de_adhesivos_gs1
integer x = 631
integer y = 300
integer height = 88
integer taborder = 20
end type

on iuo_cliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type iuo_planta from uo_seleccion_plantas within w_impresion_de_adhesivos_gs1
integer x = 631
integer y = 176
integer height = 88
integer taborder = 10
end type

on iuo_planta.destroy
call uo_seleccion_plantas::destroy
end on

type dw_1 from datawindow within w_impresion_de_adhesivos_gs1
integer x = 82
integer y = 708
integer width = 2898
integer height = 1188
integer taborder = 50
boolean titlebar = true
string title = "Pallets Seleccionados"
string dataobject = "dw_mues_seleccion_pallets_gs1"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;This.SelectRow(Row, Not This.IsSelected(Row))
end event

type st_5 from statictext within w_impresion_de_adhesivos_gs1
boolean visible = false
integer x = 1623
integer y = 184
integer width = 247
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_4 from statictext within w_impresion_de_adhesivos_gs1
integer x = 302
integer y = 312
integer width = 247
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_impresion_de_adhesivos_gs1
integer x = 302
integer y = 152
integer width = 247
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_impresion_de_adhesivos_gs1
integer x = 41
integer y = 576
integer width = 2994
integer height = 1356
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_encabe from statictext within w_impresion_de_adhesivos_gs1
integer x = 41
integer y = 44
integer width = 2994
integer height = 476
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_nuevo from picturebutton within w_impresion_de_adhesivos_gs1
integer x = 3122
integer y = 620
integer width = 315
integer height = 260
integer taborder = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\NuevoEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\NuevoDisab.png"
alignment htextalign = left!
end type

event clicked;parent.triggerevent("ue_nuevo")

sle_movto.Text	=	''
end event

type pb_salir from picturebutton within w_impresion_de_adhesivos_gs1
integer x = 3163
integer y = 1752
integer width = 233
integer height = 196
integer taborder = 100
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\sa"
string disabledname = "\Desarrollo 12\Imagenes\Botones\SalirDisab.png"
alignment htextalign = left!
end type

event clicked;Close(parent)
end event

type pb_imprimir from picturebutton within w_impresion_de_adhesivos_gs1
integer x = 3122
integer y = 904
integer width = 315
integer height = 288
integer taborder = 90
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Imprimir.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Imprimir-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEVent("ue_imprimir")
end event

type gb_3 from groupbox within w_impresion_de_adhesivos_gs1
boolean visible = false
integer x = 3109
integer y = 1672
integer width = 256
integer height = 248
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type gb_2 from groupbox within w_impresion_de_adhesivos_gs1
boolean visible = false
integer x = 3118
integer y = 756
integer width = 256
integer height = 548
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type gb_1 from groupbox within w_impresion_de_adhesivos_gs1
boolean visible = false
integer x = 3141
integer y = 152
integer width = 256
integer height = 224
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

