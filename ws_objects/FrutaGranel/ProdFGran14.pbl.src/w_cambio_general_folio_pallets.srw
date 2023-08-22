$PBExportHeader$w_cambio_general_folio_pallets.srw
forward
global type w_cambio_general_folio_pallets from window
end type
type pb_2 from picturebutton within w_cambio_general_folio_pallets
end type
type pb_1 from picturebutton within w_cambio_general_folio_pallets
end type
type cbx_visible from checkbox within w_cambio_general_folio_pallets
end type
type em_copias from editmask within w_cambio_general_folio_pallets
end type
type st_copias from statictext within w_cambio_general_folio_pallets
end type
type dw_2 from datawindow within w_cambio_general_folio_pallets
end type
type pb_lectura from picturebutton within w_cambio_general_folio_pallets
end type
type iuo_especie from uo_seleccion_especie within w_cambio_general_folio_pallets
end type
type iuo_cliente from uo_seleccion_clientesprod within w_cambio_general_folio_pallets
end type
type iuo_planta from uo_seleccion_plantas within w_cambio_general_folio_pallets
end type
type dw_1 from datawindow within w_cambio_general_folio_pallets
end type
type st_5 from statictext within w_cambio_general_folio_pallets
end type
type st_4 from statictext within w_cambio_general_folio_pallets
end type
type st_3 from statictext within w_cambio_general_folio_pallets
end type
type st_2 from statictext within w_cambio_general_folio_pallets
end type
type st_encabe from statictext within w_cambio_general_folio_pallets
end type
type pb_nuevo from picturebutton within w_cambio_general_folio_pallets
end type
type pb_salir from picturebutton within w_cambio_general_folio_pallets
end type
type pb_imprimir from picturebutton within w_cambio_general_folio_pallets
end type
end forward

global type w_cambio_general_folio_pallets from window
integer width = 4814
integer height = 2404
boolean titlebar = true
string title = "CAMBIO DE TARJA DE PALLETS"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 16777215
string icon = "AppIcon!"
event ue_nuevo ( )
event ue_recuperadatos ( )
event ue_imprimir ( )
pb_2 pb_2
pb_1 pb_1
cbx_visible cbx_visible
em_copias em_copias
st_copias st_copias
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
global w_cambio_general_folio_pallets w_cambio_general_folio_pallets

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

iuo_cliente.Codigo									=	gi_CodExport
iuo_cliente.dw_seleccion.Object.codigo[1]	=	iuo_cliente.Codigo

iuo_planta.Codigo									=	gstr_ParamPlanta.CodigoPlanta
iuo_planta.dw_seleccion.Object.codigo[1]	=	iuo_planta.Codigo

iuo_especie.seleccion(True, False)
iuo_especie.Todos(True)
iuo_especie.Enabled	=	True

dw_1.Reset()

pb_1.Enabled			=	False
pb_2.Enabled			=	False
pb_imprimir.Enabled	=	False

end event

event ue_recuperadatos();Integer	respuesta, li_procedencia

li_procedencia = 1

DO
	IF dw_1.Retrieve(iuo_planta.codigo, iuo_cliente.codigo, iuo_especie.codigo, &
						  li_procedencia, -1) = -1 THEN

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
			
			iuo_cliente.Bloquear(True)
			iuo_planta.Bloquear(True)
			iuo_especie.Enabled	=	False
			
		END IF
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir();Integer 	li_fila, li_cliente
Long		ll_pallet, ll_planta, ll_antiguo

str_mant	lstr_mant

FOR li_fila = 1 TO dw_1.RowCount()
	IF dw_1.IsSelected(li_fila) THEN
		lstr_mant.Argumento[1]	=	String(iuo_cliente.codigo)
		lstr_mant.Argumento[2]	=	String(iuo_planta.codigo)
		lstr_mant.Argumento[3]	=	String(dw_1.Object.paen_numero[li_fila])
		
		OpenWithParm(w_cambio_folio_pallets, lstr_mant)
		
		lstr_mant = Message.PowerObjectParm

		li_cliente		=	Integer(lstr_mant.Argumento[1])
		ll_planta		=	Long(lstr_mant.Argumento[2])
		ll_antiguo	=	Long(lstr_mant.Argumento[3])
		ll_pallet		=	Long(lstr_mant.Argumento[4])

		IF ll_pallet > 0 AND NOT IsNull(ll_pallet) THEN
			DECLARE Cambio_Folio_pallet PROCEDURE FOR dbo.fgran_cambio_folio_pallet  
					@cliente	= :li_cliente,   
					@planta 	= :ll_planta,   
					@pallet 	= :ll_antiguo,   
					@nuevo 	= :ll_pallet  
				USING SQLCA;
				
				EXECUTE Cambio_Folio_pallet;
				
				IF sqlca.SQLCode = -1 THEN
					F_errorbasedatos(sqlca,"Actualización de Folio de Pallet")
					Rollback;
				ELSE
					Commit;
				END IF
				CLOSE Cambio_Folio_pallet;
		END IF
	END IF
NEXT

TriggerEvent("ue_recuperadatos")
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

on w_cambio_general_folio_pallets.create
this.pb_2=create pb_2
this.pb_1=create pb_1
this.cbx_visible=create cbx_visible
this.em_copias=create em_copias
this.st_copias=create st_copias
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
this.cbx_visible,&
this.em_copias,&
this.st_copias,&
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

on w_cambio_general_folio_pallets.destroy
destroy(this.pb_2)
destroy(this.pb_1)
destroy(this.cbx_visible)
destroy(this.em_copias)
destroy(this.st_copias)
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
	
TriggerEVent("ue_nuevo")
end event

event resize;Integer	maximo, li_posic_x, li_posic_y,  &
			li_Ancho = 300, li_Alto = 245, li_Siguiente = 255
			
dw_1.Resize(This.WorkSpaceWidth() - 600,This.WorkSpaceHeight() - dw_1.y - 75)

st_2.Resize(dw_1.Width, dw_1.Height + 160)

st_encabe.width		= st_2.width
st_encabe.x				= st_2.x

li_posic_x				=	This.WorkSpaceWidth() - 400
li_posic_y				=	30 

dw_1.width				= dw_1.width - 82
dw_1.Height				= dw_1.Height - 41

IF pb_lectura.Visible THEN
	pb_lectura.x				=	li_posic_x
	pb_lectura.y				=	li_posic_y
	pb_lectura.width		=	li_Ancho
	pb_lectura.height		=	li_Alto
	li_posic_y += li_Siguiente
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				=	li_posic_x
	pb_nuevo.y				=	li_posic_y
	pb_nuevo.width			=	li_Ancho
	pb_nuevo.height		=	li_Alto
	li_posic_y += li_Siguiente
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_posic_y += li_Siguiente
END IF


pb_salir.x		= li_posic_x
pb_salir.y		= dw_1.y + dw_1.Height - pb_Salir.Height
pb_salir.width	=	li_Ancho
pb_salir.height	=	li_Alto
end event

type pb_2 from picturebutton within w_cambio_general_folio_pallets
integer x = 562
integer y = 376
integer width = 402
integer height = 136
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

type pb_1 from picturebutton within w_cambio_general_folio_pallets
integer x = 155
integer y = 376
integer width = 402
integer height = 136
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

type cbx_visible from checkbox within w_cambio_general_folio_pallets
boolean visible = false
integer x = 2949
integer y = 404
integer width = 672
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Formato Completo"
boolean checked = true
end type

event clicked;IF THIS.Checked THEN
	THIS.Text = 'Formato Completo'
	
ELSE
	THIS.Text = 'Solo Datos'
	
END IF
end event

type em_copias from editmask within w_cambio_general_folio_pallets
boolean visible = false
integer x = 3913
integer y = 392
integer width = 201
integer height = 92
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "00"
borderstyle borderstyle = stylelowered!
string mask = "00"
boolean spin = true
double increment = 1
string minmax = "1~~5"
end type

type st_copias from statictext within w_cambio_general_folio_pallets
boolean visible = false
integer x = 3666
integer y = 404
integer width = 229
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Copias"
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_cambio_general_folio_pallets
boolean visible = false
integer x = 37
integer y = 52
integer width = 498
integer height = 344
integer taborder = 110
string title = "none"
string dataobject = "dw_adhesivos_pallets"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type pb_lectura from picturebutton within w_cambio_general_folio_pallets
integer x = 4421
integer y = 120
integer width = 302
integer height = 244
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Busqueda.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Busqueda-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;parent.triggerevent("ue_recuperadatos")
end event

type iuo_especie from uo_seleccion_especie within w_cambio_general_folio_pallets
integer x = 2313
integer y = 160
integer taborder = 30
end type

on iuo_especie.destroy
call uo_seleccion_especie::destroy
end on

type iuo_cliente from uo_seleccion_clientesprod within w_cambio_general_folio_pallets
integer x = 1024
integer y = 252
integer height = 88
integer taborder = 20
end type

on iuo_cliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type iuo_planta from uo_seleccion_plantas within w_cambio_general_folio_pallets
integer x = 1024
integer y = 128
integer height = 88
integer taborder = 10
end type

on iuo_planta.destroy
call uo_seleccion_plantas::destroy
end on

type dw_1 from datawindow within w_cambio_general_folio_pallets
integer x = 55
integer y = 508
integer width = 4197
integer height = 1552
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

type st_5 from statictext within w_cambio_general_folio_pallets
integer x = 1984
integer y = 256
integer width = 247
integer height = 64
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

type st_4 from statictext within w_cambio_general_folio_pallets
integer x = 695
integer y = 264
integer width = 247
integer height = 64
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

type st_3 from statictext within w_cambio_general_folio_pallets
integer x = 695
integer y = 140
integer width = 247
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_cambio_general_folio_pallets
integer x = 23
integer y = 372
integer width = 4251
integer height = 1708
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

type st_encabe from statictext within w_cambio_general_folio_pallets
integer x = 27
integer y = 44
integer width = 4251
integer height = 332
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

type pb_nuevo from picturebutton within w_cambio_general_folio_pallets
integer x = 4421
integer y = 588
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
long backcolor = 553648127
end type

event clicked;parent.triggerevent("ue_nuevo")

end event

type pb_salir from picturebutton within w_cambio_general_folio_pallets
integer x = 4421
integer y = 1988
integer width = 302
integer height = 244
integer taborder = 100
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
end type

event clicked;Close(parent)
end event

type pb_imprimir from picturebutton within w_cambio_general_folio_pallets
string tag = "Cambio de Folio"
integer x = 4421
integer y = 868
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
string picturename = "FullBuild!"
string disabledname = "FullBuild!"
alignment htextalign = left!
string powertiptext = "Cambio de Folio"
long backcolor = 553648127
end type

event clicked;Parent.TriggerEVent("ue_imprimir")
end event

