$PBExportHeader$w_emision_ventana_pallets.srw
forward
global type w_emision_ventana_pallets from window
end type
type pb_2 from picturebutton within w_emision_ventana_pallets
end type
type pb_1 from picturebutton within w_emision_ventana_pallets
end type
type sle_movto from singlelineedit within w_emision_ventana_pallets
end type
type st_1 from statictext within w_emision_ventana_pallets
end type
type dw_2 from datawindow within w_emision_ventana_pallets
end type
type pb_lectura from picturebutton within w_emision_ventana_pallets
end type
type iuo_especie from uo_seleccion_especie within w_emision_ventana_pallets
end type
type iuo_cliente from uo_seleccion_clientesprod within w_emision_ventana_pallets
end type
type iuo_planta from uo_seleccion_plantas within w_emision_ventana_pallets
end type
type dw_1 from datawindow within w_emision_ventana_pallets
end type
type st_5 from statictext within w_emision_ventana_pallets
end type
type st_4 from statictext within w_emision_ventana_pallets
end type
type st_3 from statictext within w_emision_ventana_pallets
end type
type st_2 from statictext within w_emision_ventana_pallets
end type
type st_encabe from statictext within w_emision_ventana_pallets
end type
type pb_nuevo from picturebutton within w_emision_ventana_pallets
end type
type pb_salir from picturebutton within w_emision_ventana_pallets
end type
type pb_imprimir from picturebutton within w_emision_ventana_pallets
end type
end forward

global type w_emision_ventana_pallets from window
integer width = 3698
integer height = 2264
boolean titlebar = true
string title = "EMISION DE ADHESIVOS PARA VENTANA DE PALLETS"
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
pb_2 pb_2
pb_1 pb_1
sle_movto sle_movto
st_1 st_1
dw_2 dw_2
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
end type
global w_emision_ventana_pallets w_emision_ventana_pallets

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
//iuo_planta.dw_seleccion.Object.codigo[1]			=	iuo_planta.Codigo
iuo_planta.TriggerEvent("ue_cambio")

iuo_especie.seleccion(True, False)
iuo_especie.Todos(True)
iuo_especie.Enabled	=	True

dw_1.Reset()

pb_1.Enabled			=	False
pb_2.Enabled			=	False
pb_imprimir.Enabled	=	False

end event

event ue_recuperadatos();Integer	respuesta
Long		ll_movto


ll_movto	=	Long(sle_movto.Text)

DO
	IF dw_1.Retrieve(iuo_planta.codigo, &
						  iuo_cliente.codigo, &
						  iuo_especie.codigo, &
						  ii_procedencia, ll_movto) = -1 THEN

		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		IF dw_1.RowCount() < 1 THEN
			Messagebox("Advertencia", "No existen pallets para los criterios ingresados")
			PostEvent("ue_nuevo")
		ELSE
			pb_imprimir.Enabled	=	True
			pb_1.Enabled			=	True
			pb_2.Enabled			=	True
			
//			iuo_cliente.Bloquear(True)
//			iuo_planta.Bloquear(True)
			iuo_especie.Enabled	=	False
			
		END IF
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir();Integer	Fila, li_fila, li_codigo, li_pagina, li_nueva
String		ls_EspVar, ls_Cal, ls_Emba, ls_Folio, ls_Cajas
//
//FOR li_fila = 1 TO dw_1.RowCount()
//	IF dw_1.IsSelected(li_fila) THEN
//		ls_codigo	=	cargacodigo(iuo_cliente.codigo, iuo_planta.codigo, &
//											dw_1.Object.paen_numero[li_fila], ii_procedencia)
//											
//		FOR li_pagina = 1 TO 2
//			li_nueva											=	dw_2.InsertRow(0)
//			dw_2.Object.ole_codigo1.Object.Text 	=	ls_codigo
//			dw_2.Object.ole_codigo2.Object.Text 	=	ls_codigo
//			dw_2.Object.ole_codigo3.Object.Text 	=	ls_codigo
//			
//			IF li_pagina = 2 THEN
//				dw_2.Object.ole_codigo3.visible		=	False
//			ELSE
//				dw_2.Object.ole_codigo3.visible		=	True
//			END IF
//			
//			dw_2.Print()
//			dw_2.Reset()
//			
//		NEXT
//	END IF
//NEXT

SetPointer(HourGlass!)



FOR li_fila = 1 TO dw_1.RowCount()
	IF dw_1.IsSelected(li_fila) THEN
		fila	=	dw_2.Retrieve( iuo_planta.codigo,iuo_cliente.codigo, &
									dw_1.Object.paen_numero[li_fila], ii_procedencia)
									
//			ls_EspVar	=	dw_2.Object.espe_codigo[fila]+ dw_2.Object.vari_codigo[fila]
//			ls_Cal			=	dw_2.Object.pafr_calibr[fila]
//			ls_Emba		=	dw_2.Object.emba_codigo[fila]
//			ls_Folio		=  String(dw_2.Object.Folio[fila])
//			ls_Cajas		=	String(dw_2.Object.cajas[fila])
//			
//			dw_2.Object.ole_espvar.Object.Text 	=	ls_EspVar
//			dw_2.Object.ole_emba.Object.Text 	=	ls_Emba
//			dw_2.Object.ole_cal.Object.Text 		=	ls_Cal
//			dw_2.Object.ole_cajas.Object.Text 	=	ls_Cajas
//			dw_2.Object.ole_folio.Object.Text 	=	ls_Folio
		
		IF fila = -1 THEN
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		ELSEIF fila = 0 THEN
			MessageBox( "No Existe información", "No existe información para este informe.", &
							 StopSign!, Ok!)
		ELSE
			FOR li_pagina = 1 TO 4
				dw_2.Print()
			NEXT	
		END IF
	END IF
NEXT

SetPointer(Arrow!)			
end event

public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia);String ls_respuesta

DECLARE Codigo PROCEDURE FOR dba.genera_adhesivos_pallets  
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

on w_emision_ventana_pallets.create
this.pb_2=create pb_2
this.pb_1=create pb_1
this.sle_movto=create sle_movto
this.st_1=create st_1
this.dw_2=create dw_2
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
this.Control[]={this.pb_2,&
this.pb_1,&
this.sle_movto,&
this.st_1,&
this.dw_2,&
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
this.pb_imprimir}
end on

on w_emision_ventana_pallets.destroy
destroy(this.pb_2)
destroy(this.pb_1)
destroy(this.sle_movto)
destroy(this.st_1)
destroy(this.dw_2)
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
end on

event open;dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

ii_procedencia				=	Integer(Message.StringParm)

TriggerEVent("ue_nuevo")
end event

event resize;Integer		li_posi_y, li_objeto

dw_1.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - dw_1.y)

dw_1.x					= 78
st_2.X					= dw_1.x
pb_1.x					= dw_1.x + 165
pb_2.x					= pb_1.x + pb_1.width

st_2.Resize(dw_1.Width, dw_1.Height + 50)

dw_1.x			+= 50
dw_1.Height 	-= 250
dw_1.Width		-= 100

st_encabe.width		= st_2.width
st_encabe.x			= st_2.x

pb_lectura.x			= This.WorkSpaceWidth() - 350
pb_lectura.y			= st_encabe.y + 88

pb_nuevo.x				= pb_lectura.x
pb_imprimir.x			= pb_lectura.x
pb_salir.x				= pb_lectura.x

li_posi_y	= st_2.y - 92

IF pb_nuevo.Visible THEN
	li_objeto	++
	li_posi_y	+= 255
	pb_nuevo.y	= li_posi_y
END IF

IF pb_imprimir.Visible THEN
	li_objeto	++
	li_posi_y	+= 255
	pb_imprimir.y	= li_posi_y
END IF

pb_salir.y				= st_2.y + st_2.height - 255
end event

type pb_2 from picturebutton within w_emision_ventana_pallets
integer x = 535
integer y = 592
integer width = 402
integer height = 224
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\btn_ninguno_on.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\btn_ninguno_off.png"
alignment htextalign = left!
end type

event clicked;dw_1.SelectRow(0, False)
end event

type pb_1 from picturebutton within w_emision_ventana_pallets
integer x = 96
integer y = 592
integer width = 402
integer height = 224
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\btn_todos_on.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\btn_todos_off.png"
alignment htextalign = left!
end type

event clicked;dw_1.SelectRow(0, True)
end event

type sle_movto from singlelineedit within w_emision_ventana_pallets
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
string facename = "Arial"
long textcolor = 33554432
integer limit = 8
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_emision_ventana_pallets
integer x = 1623
integer y = 312
integer width = 352
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Movimiento"
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_emision_ventana_pallets
boolean visible = false
integer x = 50
integer y = 52
integer width = 201
integer height = 152
integer taborder = 110
string title = "none"
string dataobject = "dw_adhesivo_ventana_pallet"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type pb_lectura from picturebutton within w_emision_ventana_pallets
integer x = 3186
integer y = 76
integer width = 300
integer height = 245
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
alignment htextalign = left!
end type

event clicked;parent.triggerevent("ue_recuperadatos")
end event

type iuo_especie from uo_seleccion_especie within w_emision_ventana_pallets
integer x = 1979
integer y = 88
integer taborder = 30
end type

on iuo_especie.destroy
call uo_seleccion_especie::destroy
end on

type iuo_cliente from uo_seleccion_clientesprod within w_emision_ventana_pallets
integer x = 631
integer y = 300
integer height = 88
integer taborder = 20
end type

on iuo_cliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type iuo_planta from uo_seleccion_plantas within w_emision_ventana_pallets
integer x = 631
integer y = 176
integer height = 88
integer taborder = 10
end type

on iuo_planta.destroy
call uo_seleccion_plantas::destroy
end on

type dw_1 from datawindow within w_emision_ventana_pallets
integer x = 82
integer y = 824
integer width = 2898
integer height = 1268
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

type st_5 from statictext within w_emision_ventana_pallets
integer x = 1623
integer y = 184
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

type st_4 from statictext within w_emision_ventana_pallets
integer x = 302
integer y = 312
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

type st_3 from statictext within w_emision_ventana_pallets
integer x = 302
integer y = 152
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

type st_2 from statictext within w_emision_ventana_pallets
integer x = 41
integer y = 576
integer width = 2994
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

type st_encabe from statictext within w_emision_ventana_pallets
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

type pb_nuevo from picturebutton within w_emision_ventana_pallets
integer x = 3191
integer y = 576
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

type pb_salir from picturebutton within w_emision_ventana_pallets
integer x = 3173
integer y = 1864
integer width = 300
integer height = 245
integer taborder = 100
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
end type

event clicked;Close(parent)
end event

type pb_imprimir from picturebutton within w_emision_ventana_pallets
integer x = 3195
integer y = 852
integer width = 300
integer height = 245
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

