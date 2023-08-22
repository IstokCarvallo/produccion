$PBExportHeader$w_emision_adhesivos_anexos.srw
forward
global type w_emision_adhesivos_anexos from window
end type
type dw_3 from datawindow within w_emision_adhesivos_anexos
end type
type dw_2 from datawindow within w_emision_adhesivos_anexos
end type
type cb_2 from commandbutton within w_emision_adhesivos_anexos
end type
type cb_1 from commandbutton within w_emision_adhesivos_anexos
end type
type pb_lectura from picturebutton within w_emision_adhesivos_anexos
end type
type iuo_especie from uo_seleccion_especie within w_emision_adhesivos_anexos
end type
type iuo_cliente from uo_seleccion_clientesprod within w_emision_adhesivos_anexos
end type
type iuo_planta from uo_seleccion_plantas within w_emision_adhesivos_anexos
end type
type dw_1 from datawindow within w_emision_adhesivos_anexos
end type
type st_5 from statictext within w_emision_adhesivos_anexos
end type
type st_4 from statictext within w_emision_adhesivos_anexos
end type
type st_3 from statictext within w_emision_adhesivos_anexos
end type
type st_2 from statictext within w_emision_adhesivos_anexos
end type
type st_encabe from statictext within w_emision_adhesivos_anexos
end type
type pb_nuevo from picturebutton within w_emision_adhesivos_anexos
end type
type pb_salir from picturebutton within w_emision_adhesivos_anexos
end type
type pb_imprimir from picturebutton within w_emision_adhesivos_anexos
end type
type gb_3 from groupbox within w_emision_adhesivos_anexos
end type
type gb_2 from groupbox within w_emision_adhesivos_anexos
end type
type gb_1 from groupbox within w_emision_adhesivos_anexos
end type
end forward

global type w_emision_adhesivos_anexos from window
integer width = 5152
integer height = 2236
boolean titlebar = true
string title = "EMISION DE ADHESIVOS ANEXOS PARA VENTANA DE PALLETS"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 16777215
string icon = "AppIcon!"
boolean center = true
event ue_nuevo ( )
event ue_recuperadatos ( )
event ue_imprimir ( )
dw_3 dw_3
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
global w_emision_adhesivos_anexos w_emision_adhesivos_anexos

type variables
Integer		ii_procedencia
end variables

forward prototypes
public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia)
end prototypes

event ue_nuevo();iuo_cliente.seleccion(False, False)
iuo_planta.seleccion(False, False)

iuo_cliente.Bloquear(False)
iuo_planta.Bloquear(False)

iuo_cliente.Codigo										=	gi_CodExport
iuo_cliente.dw_seleccion.Object.codigo[1]			=	iuo_cliente.Codigo

iuo_planta.Codigo											=	gstr_ParamPlanta.CodigoPlanta
iuo_planta.dw_seleccion.Object.codigo[1]	=	iuo_planta.Codigo

iuo_especie.seleccion(True, False)
iuo_especie.Todos(True)
iuo_especie.Enabled	=	True

dw_1.Reset()

cb_1.Enabled			=	False
cb_2.Enabled			=	False
pb_imprimir.Enabled	=	False

end event

event ue_recuperadatos();Integer	respuesta
DO
	IF dw_1.Retrieve(iuo_planta.codigo, &
						  iuo_cliente.codigo, &
						  iuo_especie.codigo, &
						  ii_procedencia, -1) = -1 THEN

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
			
			iuo_cliente.Bloquear(True)
			iuo_planta.Bloquear(True)
			iuo_especie.Enabled	=	False
			
		END IF
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir();Integer		li_fila, li_codigo, li_pagina, li_nueva, li_especie
String		ls_codigo, ls_embalaje, ls_cajas, ls_productor, ls_calibre, ls_espnoming
uo_especie 	luo_especie

luo_especie 	=	Create uo_especie

FOR li_fila = 1 TO dw_1.RowCount()
	IF dw_1.IsSelected(li_fila) THEN
		ls_codigo		=	cargacodigo(iuo_cliente.codigo, iuo_planta.codigo, &
											   dw_1.Object.paen_numero[li_fila], ii_procedencia)
												
		li_especie 		=	dw_1.Object.espe_codigo[li_fila]
		luo_especie.Existe(li_especie, True, sqlca)
		ls_espnoming	=	luo_especie.Nombre_ingles
		
		ls_embalaje		=	Mid(ls_codigo, 41, 4)
		ls_cajas			=	String(Integer(Mid(ls_codigo, 52, 3)))
		ls_productor	=	String(long(Mid(ls_codigo, 27, 4)))
		ls_calibre		=	Trim(Mid(ls_codigo, 45, 3))
		
		FOR li_pagina = 1 TO 5
			li_nueva									=	dw_3.InsertRow(0)
			dw_3.Object.Ole_1.Object.Text 	= 	ls_embalaje
			dw_3.Object.codigo[li_nueva] 		= 	ls_embalaje
			dw_3.Print()
			dw_3.Reset()
		NEXT
		
		FOR li_pagina = 1 TO 5
			li_nueva									=	dw_3.InsertRow(0)
			dw_3.Object.Ole_1.Object.Text 	= 	ls_cajas
			dw_3.Object.codigo[li_nueva] 		= 	ls_cajas
			dw_3.Print()
			dw_3.Reset()
		NEXT
		
		FOR li_pagina = 1 TO 5
			li_nueva									=	dw_3.InsertRow(0)
			dw_3.Object.Ole_1.Object.Text 	= 	ls_productor
			dw_3.Object.codigo[li_nueva] 		= 	ls_productor
			dw_3.Print()
			dw_3.Reset()
		NEXT
		
		FOR li_pagina = 1 TO 5
			li_nueva									=	dw_3.InsertRow(0)
			dw_3.Object.Ole_1.Object.Text 	= 	ls_calibre
			dw_3.Object.codigo[li_nueva] 		= 	ls_calibre
			dw_3.Print()
			dw_3.Reset()
		NEXT
		
		FOR li_pagina = 1 TO 5
			li_nueva									=	dw_2.InsertRow(0)
			dw_2.Object.codigo[li_nueva] 		= 	ls_espnoming
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
	F_ErrorBaseDatos(SQLCA, "Lectura del Procedimiento Almacenado genera_adhesivos_pallets" )
ELSE
	FEtCH Codigo INTO :ls_respuesta;
END IF	
	
CLOSE Codigo;

RETURN ls_respuesta 

end function

on w_emision_adhesivos_anexos.create
this.dw_3=create dw_3
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
this.Control[]={this.dw_3,&
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

on w_emision_adhesivos_anexos.destroy
destroy(this.dw_3)
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

type dw_3 from datawindow within w_emision_adhesivos_anexos
boolean visible = false
integer x = 50
integer y = 260
integer width = 709
integer height = 200
integer taborder = 30
string title = "none"
string dataobject = "dw_adhesivo_ventana_pallet_70x30"
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_emision_adhesivos_anexos
boolean visible = false
integer x = 50
integer y = 52
integer width = 709
integer height = 200
integer taborder = 110
string title = "none"
string dataobject = "dw_adhesivo_ventana_pallet_60x20"
borderstyle borderstyle = stylelowered!
end type

type cb_2 from commandbutton within w_emision_adhesivos_anexos
integer x = 608
integer y = 560
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

type cb_1 from commandbutton within w_emision_adhesivos_anexos
integer x = 206
integer y = 560
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

type pb_lectura from picturebutton within w_emision_adhesivos_anexos
integer x = 4599
integer y = 124
integer width = 302
integer height = 244
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Buscar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Buscar-bn.png"
alignment htextalign = left!
end type

event clicked;parent.triggerevent("ue_recuperadatos")
end event

type iuo_especie from uo_seleccion_especie within w_emision_adhesivos_anexos
integer x = 2117
integer y = 324
integer taborder = 30
end type

on iuo_especie.destroy
call uo_seleccion_especie::destroy
end on

type iuo_cliente from uo_seleccion_clientesprod within w_emision_adhesivos_anexos
integer x = 2117
integer y = 220
integer height = 88
integer taborder = 20
end type

on iuo_cliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type iuo_planta from uo_seleccion_plantas within w_emision_adhesivos_anexos
integer x = 2117
integer y = 96
integer height = 88
integer taborder = 10
end type

on iuo_planta.destroy
call uo_seleccion_plantas::destroy
end on

type dw_1 from datawindow within w_emision_adhesivos_anexos
integer x = 82
integer y = 664
integer width = 4357
integer height = 1384
integer taborder = 50
boolean titlebar = true
string title = "Pallets Seleccionados"
string dataobject = "dw_mues_seleccion_pallets"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;This.SelectRow(Row, Not This.IsSelected(Row))
end event

type st_5 from statictext within w_emision_adhesivos_anexos
integer x = 1787
integer y = 412
integer width = 247
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_4 from statictext within w_emision_adhesivos_anexos
integer x = 1787
integer y = 232
integer width = 247
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_emision_adhesivos_anexos
integer x = 1787
integer y = 108
integer width = 247
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_emision_adhesivos_anexos
integer x = 41
integer y = 540
integer width = 4453
integer height = 1540
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

type st_encabe from statictext within w_emision_adhesivos_anexos
integer x = 41
integer y = 52
integer width = 4453
integer height = 484
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

type pb_nuevo from picturebutton within w_emision_adhesivos_anexos
integer x = 4599
integer y = 556
integer width = 302
integer height = 244
integer taborder = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Documento.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Documento-bn.png"
alignment htextalign = left!
end type

event clicked;parent.triggerevent("ue_nuevo")

end event

type pb_salir from picturebutton within w_emision_adhesivos_anexos
integer x = 4599
integer y = 1684
integer width = 302
integer height = 244
integer taborder = 100
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
end type

event clicked;Close(parent)
end event

type pb_imprimir from picturebutton within w_emision_adhesivos_anexos
integer x = 4613
integer y = 844
integer width = 302
integer height = 244
integer taborder = 90
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Imprimir.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Imprimir-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEVent("ue_imprimir")
end event

type gb_3 from groupbox within w_emision_adhesivos_anexos
boolean visible = false
integer x = 3127
integer y = 1860
integer width = 233
integer height = 196
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_2 from groupbox within w_emision_adhesivos_anexos
boolean visible = false
integer x = 3113
integer y = 428
integer width = 233
integer height = 196
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_1 from groupbox within w_emision_adhesivos_anexos
boolean visible = false
integer x = 3113
integer y = 48
integer width = 233
integer height = 196
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

