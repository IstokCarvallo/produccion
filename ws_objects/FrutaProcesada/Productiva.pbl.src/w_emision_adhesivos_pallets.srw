$PBExportHeader$w_emision_adhesivos_pallets.srw
forward
global type w_emision_adhesivos_pallets from window
end type
type cbx_pucho from checkbox within w_emision_adhesivos_pallets
end type
type st_7 from statictext within w_emision_adhesivos_pallets
end type
type st_6 from statictext within w_emision_adhesivos_pallets
end type
type cbx_palletdate from checkbox within w_emision_adhesivos_pallets
end type
type iuo_planta from uo_seleccion_planta_comun within w_emision_adhesivos_pallets
end type
type pb_ventanas from uo_botonventanas within w_emision_adhesivos_pallets
end type
type st_1 from statictext within w_emision_adhesivos_pallets
end type
type sle_pallet from singlelineedit within w_emision_adhesivos_pallets
end type
type em_copias from editmask within w_emision_adhesivos_pallets
end type
type st_copias from statictext within w_emision_adhesivos_pallets
end type
type pb_lectura from picturebutton within w_emision_adhesivos_pallets
end type
type iuo_especie from uo_seleccion_especie within w_emision_adhesivos_pallets
end type
type st_5 from statictext within w_emision_adhesivos_pallets
end type
type st_4 from statictext within w_emision_adhesivos_pallets
end type
type st_3 from statictext within w_emision_adhesivos_pallets
end type
type pb_nuevo from picturebutton within w_emision_adhesivos_pallets
end type
type pb_salir from picturebutton within w_emision_adhesivos_pallets
end type
type cb_2 from commandbutton within w_emision_adhesivos_pallets
end type
type cb_1 from commandbutton within w_emision_adhesivos_pallets
end type
type gb_4 from groupbox within w_emision_adhesivos_pallets
end type
type iuo_cliente from uo_seleccion_cliente_comun within w_emision_adhesivos_pallets
end type
type pb_imprimir from picturebutton within w_emision_adhesivos_pallets
end type
type cbx_visible from checkbox within w_emision_adhesivos_pallets
end type
type st_encabe from statictext within w_emision_adhesivos_pallets
end type
type dw_2 from datawindow within w_emision_adhesivos_pallets
end type
type dw_1 from datawindow within w_emision_adhesivos_pallets
end type
type st_2 from statictext within w_emision_adhesivos_pallets
end type
end forward

global type w_emision_adhesivos_pallets from window
integer width = 5239
integer height = 2176
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
cbx_pucho cbx_pucho
st_7 st_7
st_6 st_6
cbx_palletdate cbx_palletdate
iuo_planta iuo_planta
pb_ventanas pb_ventanas
st_1 st_1
sle_pallet sle_pallet
em_copias em_copias
st_copias st_copias
pb_lectura pb_lectura
iuo_especie iuo_especie
st_5 st_5
st_4 st_4
st_3 st_3
pb_nuevo pb_nuevo
pb_salir pb_salir
cb_2 cb_2
cb_1 cb_1
gb_4 gb_4
iuo_cliente iuo_cliente
pb_imprimir pb_imprimir
cbx_visible cbx_visible
st_encabe st_encabe
dw_2 dw_2
dw_1 dw_1
st_2 st_2
end type
global w_emision_adhesivos_pallets w_emision_adhesivos_pallets

type variables
Integer				ii_procedencia, ii_operacion, ii_sistema
uo_AnalizaPallet	iuo_pallet

end variables

forward prototypes
public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia)
end prototypes

event ue_nuevo();iuo_cliente.seleccion(False, False)
iuo_planta.seleccion(False, False)

iuo_cliente.Bloquear(False)
iuo_planta.Bloquear(False)

iuo_cliente.Codigo											=	gi_CodExport
iuo_cliente.dw_seleccion.Object.codigo[1]			=	iuo_cliente.Codigo

iuo_planta.Codigo											=	gi_codplanta//gstr_ParamPlanta.CodigoPlanta
iuo_planta.dw_seleccion.Object.codigo[1]			=	iuo_planta.Codigo

iuo_especie.seleccion(True, False)
iuo_especie.Todos(True)
iuo_especie.Enabled	=	True

dw_1.Reset()

cb_1.Enabled			=	False
cb_2.Enabled			=	False
pb_ventanas.Enabled	=	False



end event

event ue_recuperadatos();Integer	respuesta, li_procedencia

IF ii_procedencia = 1 OR ii_procedencia = 0 THEN
	li_procedencia = 1
	
ELSE 
	li_procedencia = ii_procedencia
	
END IF

DO
	IF dw_1.Retrieve(iuo_planta.codigo, iuo_cliente.codigo, iuo_especie.codigo, ii_sistema, -1, ii_operacion) = -1 THEN

		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		IF dw_1.RowCount() < 1 THEN
			Messagebox("Advertencia", "No existen pallets para los criterios ingresados")
			PostEvent("ue_nuevo")
			
		ELSE
			pb_ventanas.Enabled	=	False
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

event ue_imprimir();Integer	li_fila, li_codigo, li_pagina, li_nueva, li_copias, li_visible
String		ls_codigo, ls_paso, ls_pallet, ls_qr, ls_Ruta
Date		ld_fecha
uo_QR	luo_QR

luo_QR	=	Create uo_QR

If cbx_palletdate.Checked Then
	dw_2.DataObject	=	"dw_adhesivo_ventana_pallets_chico_sinbordes"
	dw_2.SetTransObject(sqlca)
	dw_2.Reset()
	li_copias	=	Integer(em_copias.Text)
	
	FOR li_fila = 1 TO dw_1.RowCount()
		If dw_1.IsSelected(li_fila) Then
			
			dw_2.Retrieve(dw_1.Object.clie_codigo[li_fila], dw_1.Object.plde_codigo[li_fila], &
							  Long(dw_1.Object.paen_numero[li_fila]), ii_sistema, li_visible)
			
			If dw_2.RowCount() > 0 Then
				
				ls_paso				=	Mid(String(dw_2.Object.paen_gs1128[1]), 21, 6)
				ld_fecha				=	Date(Right(ls_paso, 2) + "/" + Mid(ls_paso, 3, 2) + "/" + Left(ls_paso, 2))
				
				dw_2.DataObject	=	"dw_adhesivo_palletdate"
				dw_2.Reset()
				
				FOR li_pagina = 1 TO li_copias
					dw_2.InsertRow(0)
					dw_2.Object.palletdate[li_pagina]	=	ld_fecha
				NEXT
					
				dw_2.Print()
				dw_2.Reset()
			Else
				Messagebox("Advertencia", "Revise Datos del Pallet, No se pudo Imprimir Ventana")
			End If
		End If
	NEXT
Else
	If ii_procedencia <> 0 Then //Impresión EXCLUSIVAMENTE de Codigos GS1-128
	
		dw_2.DataObject	=	"dw_adhesivos_pallets"
		dw_2.SetTransObject(sqlca)
		
		FOR li_fila = 1 TO dw_1.RowCount()
			If dw_1.IsSelected(li_fila) Then
				ls_codigo	=	CargaCodigo(iuo_cliente.codigo, iuo_planta.codigo, dw_1.Object.paen_numero[li_fila], ii_sistema)
													
				FOR li_pagina = 1 TO 2
					li_nueva											=	dw_2.InsertRow(0)
					dw_2.Object.ole_codigo1.Object.Text 	=	ls_codigo
					dw_2.Object.ole_codigo2.Object.Text 	=	ls_codigo
					dw_2.Object.ole_codigo3.Object.Text 	=	ls_codigo
					
					If li_pagina = 2 Then
						dw_2.Object.ole_codigo3.visible		=	False
					Else
						dw_2.Object.ole_codigo3.visible		=	True
					End If
					
					dw_2.Print()
					dw_2.Reset()
					
				NEXT
			End If
		NEXT
	Else //Impresión de VENTANA PALLET completa
		dw_2.DataObject	=	"dw_adhesivo_ventana_pallets_chico_sinbordes"
		dw_2.SetTransObject(sqlca)
		dw_2.Reset()
		li_copias	=	Integer(em_copias.Text)
		
		If cbx_visible.Checked Then
			li_visible	=	1
		Else
			li_visible	=	0
		End If
		
		FOR li_fila = 1 TO dw_1.RowCount()
			If dw_1.IsSelected(li_fila) Then
				FOR li_pagina = 1 TO li_copias
					dw_2.Retrieve(dw_1.Object.clie_codigo[li_fila], dw_1.Object.plde_codigo[li_fila], Long(dw_1.Object.paen_numero[li_fila]), ii_sistema, li_visible)
					
					If dw_2.RowCount() > 0 Then
						ls_paso								=	String(dw_2.Object.clie_codigo[1]) + String(dw_2.Object.paen_numero[1], '0000000')
						dw_2.Object.pallet.Object.Text	=	ls_paso
						ls_paso								=	String(dw_2.Object.paen_sscc18[1])
						If ls_paso = '' Then
							ls_pallet	=	String(dw_1.Object.clie_codigo[li_fila], '000') + String(dw_1.Object.paen_numero[li_fila], '000000')
							If iuo_pallet.analiza_datos(ls_pallet, SqlCa) Then ls_paso =	iuo_pallet.CodBarra
						End If
						
						dw_2.Object.sscc.Object.Text				=	ls_paso
						dw_2.Object.sscc_1.Object.Text			=	ls_paso
						dw_2.Object.sscc_2.Object.Text			=	ls_paso
						
						ls_paso											=	String(dw_2.Object.paen_gs1128[1])
						
						If ls_paso = '' Then
							ls_paso =	CargaCodigo(dw_1.Object.clie_codigo[li_fila], dw_1.Object.plde_codigo[li_fila], dw_1.Object.paen_numero[li_fila], ii_sistema)
						End If	
						dw_2.Object.gs1.Object.Text	=	ls_paso
						
						ls_QR =String(dw_2.Object.clie_codigo[1]) + String(dw_2.Object.paen_numero[1], '0000000')
		
						ls_Ruta = luo_QR.of_genera_qr(ls_QR)
						dw_2.Object.p_qrcode.FileName = ls_Ruta
							
						dw_2.Print()
						dw_2.Reset()
					End If
				NEXT
			End If
		NEXT
	End If
End If

Destroy luo_QR
end event

public function string cargacodigo (integer ai_cliente, long al_planta, long al_pallet, integer ai_procedencia);String ls_respuesta

DECLARE Codigo PROCEDURE FOR dbo.genera_adhesivos_pallets  
        @Planta 		= 	:al_planta,   
        @Cliente 		= 	:ai_cliente,   
        @Pallet 		= 	:al_pallet,   
        @Procedencia = 	:ai_procedencia  
	USING SQLCA;
			
EXECUTE Codigo;

If SQLCA.SQLCode = -1 Then
	F_ErrorBaseDatos(SQLCA, "Lectura del Procedimiento Almacenado genera_adhesivos_pallets" )				
Else
	FETCH Codigo INTO :ls_respuesta;
End If	
	
CLOSE Codigo;

RETURN ls_respuesta 

end function

on w_emision_adhesivos_pallets.create
this.cbx_pucho=create cbx_pucho
this.st_7=create st_7
this.st_6=create st_6
this.cbx_palletdate=create cbx_palletdate
this.iuo_planta=create iuo_planta
this.pb_ventanas=create pb_ventanas
this.st_1=create st_1
this.sle_pallet=create sle_pallet
this.em_copias=create em_copias
this.st_copias=create st_copias
this.pb_lectura=create pb_lectura
this.iuo_especie=create iuo_especie
this.st_5=create st_5
this.st_4=create st_4
this.st_3=create st_3
this.pb_nuevo=create pb_nuevo
this.pb_salir=create pb_salir
this.cb_2=create cb_2
this.cb_1=create cb_1
this.gb_4=create gb_4
this.iuo_cliente=create iuo_cliente
this.pb_imprimir=create pb_imprimir
this.cbx_visible=create cbx_visible
this.st_encabe=create st_encabe
this.dw_2=create dw_2
this.dw_1=create dw_1
this.st_2=create st_2
this.Control[]={this.cbx_pucho,&
this.st_7,&
this.st_6,&
this.cbx_palletdate,&
this.iuo_planta,&
this.pb_ventanas,&
this.st_1,&
this.sle_pallet,&
this.em_copias,&
this.st_copias,&
this.pb_lectura,&
this.iuo_especie,&
this.st_5,&
this.st_4,&
this.st_3,&
this.pb_nuevo,&
this.pb_salir,&
this.cb_2,&
this.cb_1,&
this.gb_4,&
this.iuo_cliente,&
this.pb_imprimir,&
this.cbx_visible,&
this.st_encabe,&
this.dw_2,&
this.dw_1,&
this.st_2}
end on

on w_emision_adhesivos_pallets.destroy
destroy(this.cbx_pucho)
destroy(this.st_7)
destroy(this.st_6)
destroy(this.cbx_palletdate)
destroy(this.iuo_planta)
destroy(this.pb_ventanas)
destroy(this.st_1)
destroy(this.sle_pallet)
destroy(this.em_copias)
destroy(this.st_copias)
destroy(this.pb_lectura)
destroy(this.iuo_especie)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.pb_nuevo)
destroy(this.pb_salir)
destroy(this.cb_2)
destroy(this.cb_1)
destroy(this.gb_4)
destroy(this.iuo_cliente)
destroy(this.pb_imprimir)
destroy(this.cbx_visible)
destroy(this.st_encabe)
destroy(this.dw_2)
destroy(this.dw_1)
destroy(this.st_2)
end on

event open;String	ls_prefijo
dw_1.SetTransObject(sqlca)

iuo_pallet					=	Create uo_AnalizaPallet
//0 Para Impresion de Ventana - 1 Para impresion Codigo GS1
ii_procedencia				=	Integer(Mid(Message.StringParm, 1, 1))

//1 Para Impresion - 2 Para Reimpresion
ii_operacion				=	Integer(Mid(Message.StringParm, 2, 1))

//1 Para Granel - 2 Para Procesada
ii_sistema					=	Integer(Mid(Message.StringParm, 3, 1))

IF ii_procedencia <> 0 THEN
	st_copias.Visible			=	False
	em_copias.Visible			=	False
	This.Title					=	"EMISION DE ADHESIVOS PARA VENTANA DE PALLETS"
	
	pb_imprimir.Enabled		=	True
	pb_imprimir.Visible		=	True
	
	pb_ventanas.Enabled		=	False
	pb_ventanas.Visible		=	False
	cbx_palletdate.Visible	=	False
	st_6.Visible				=	False
	st_7.Visible				=	False
	
ELSE
	st_copias.Visible			=	True
	em_copias.Visible			=	True
	
	pb_imprimir.Enabled		=	False
	pb_imprimir.Visible		=	False
	
	pb_ventanas.Enabled		=	True
	pb_ventanas.Visible		=	True
	cbx_palletdate.Visible	=	True
	st_6.Visible				=	True
	st_7.Visible				=	True
	
	IF ii_operacion = 2 THEN 
		ls_prefijo			=	"RE"
		em_copias.Text		=	'1'
		
	ELSE 
		ls_prefijo			=	""
		em_copias.Text		=	'5'
		em_copias.Enabled = False
		
	END IF
	
	IF ii_sistema <> 1 THEN
		cbx_pucho.Checked		=	False
		cbx_pucho.Visible			=	False
	END IF
	
	cbx_palletdate.TriggerEvent("Clicked")
	
	This.Title			=	ls_prefijo + "IMPRESION DE VENTANAS DE PALLETS"
END IF
	
TriggerEVent("ue_nuevo")
end event

type cbx_pucho from checkbox within w_emision_adhesivos_pallets
integer x = 3387
integer y = 288
integer width = 283
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Pucho"
boolean lefttext = true
end type

event clicked;IF This.Checked THEN
	IF ii_operacion =  2 THEN
		em_copias.Text =	'1'
		pb_ventanas.ii_impresiones		=	1
	ELSE
		em_copias.Text = '2'
		pb_ventanas.ii_impresiones		=	2
	END IF
ELSE
	IF ii_operacion =  2 THEN
		em_copias.Text = '1'
		pb_ventanas.ii_impresiones	=	1
	ELSE
		em_copias.Text = '5'
		pb_ventanas.ii_impresiones	=	5
	END IF
END IF

em_copias.TriggerEvent("modified")
//em_copias.modified()
	
end event

type st_7 from statictext within w_emision_adhesivos_pallets
integer x = 4832
integer y = 1412
integer width = 233
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Pallet"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_6 from statictext within w_emision_adhesivos_pallets
integer x = 4832
integer y = 1484
integer width = 233
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Date"
alignment alignment = center!
boolean focusrectangle = false
end type

type cbx_palletdate from checkbox within w_emision_adhesivos_pallets
integer x = 4914
integer y = 1324
integer width = 82
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

event clicked;IF cbx_palletdate.Checked THEN
	pb_ventanas.ii_palletdate	=	1
ELSE
	pb_ventanas.ii_palletdate	=	0
END IF
	
end event

type iuo_planta from uo_seleccion_planta_comun within w_emision_adhesivos_pallets
integer x = 1143
integer y = 156
integer height = 88
integer taborder = 50
end type

on iuo_planta.destroy
call uo_seleccion_planta_comun::destroy
end on

type pb_ventanas from uo_botonventanas within w_emision_adhesivos_pallets
integer x = 4791
integer y = 1024
integer width = 302
integer height = 244
integer taborder = 100
string picturename = "\Desarrollo 17\Imagenes\Botones\Adhesivo.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Adhesivo-bn.png"
end type

event clicked;iuo_cliente.Existe(ii_cliente, False, Sqlca)

IF iuo_cliente.clie_ctlven = 1 OR ii_palletdate = 1 THEN
	CHOOSE CASE ii_operacion 
		CASE 1 
			Impresion()
			
		CASE 2
			ReImpresion()
	END CHOOSE
ELSE
	MessageBox("Protección de Sistema", "Dados los parametros ingresados para el cliente,~r~n" + &
													"no es posible gestionar Ventanas de Pallets", StopSign!)
	
END IF
end event

type st_1 from statictext within w_emision_adhesivos_pallets
integer x = 2523
integer y = 292
integer width = 242
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Folio"
boolean focusrectangle = false
end type

type sle_pallet from singlelineedit within w_emision_adhesivos_pallets
event ue_busqueda pbm_keydown
integer x = 2866
integer y = 280
integer width = 402
integer height = 92
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event ue_busqueda;IF This.Text = "" THEN
	dw_1.SetFilter("")
ELSE
	dw_1.SetFilter("String(paen_numero, '00000000') like '%" + This.Text + "%'")
END IF

dw_1.Filter()
end event

type em_copias from editmask within w_emision_adhesivos_pallets
integer x = 3922
integer y = 280
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

event modified;pb_ventanas.ii_impresiones	=	Integer(em_copias.Text)
end event

type st_copias from statictext within w_emision_adhesivos_pallets
integer x = 3698
integer y = 292
integer width = 215
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

type pb_lectura from picturebutton within w_emision_adhesivos_pallets
integer x = 4791
integer y = 148
integer width = 302
integer height = 244
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

type iuo_especie from uo_seleccion_especie within w_emision_adhesivos_pallets
integer x = 2857
integer y = 72
integer taborder = 30
end type

on iuo_especie.destroy
call uo_seleccion_especie::destroy
end on

type st_5 from statictext within w_emision_adhesivos_pallets
integer x = 2528
integer y = 164
integer width = 270
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

type st_4 from statictext within w_emision_adhesivos_pallets
integer x = 814
integer y = 292
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

type st_3 from statictext within w_emision_adhesivos_pallets
integer x = 814
integer y = 168
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

type pb_nuevo from picturebutton within w_emision_adhesivos_pallets
integer x = 4791
integer y = 512
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

type pb_salir from picturebutton within w_emision_adhesivos_pallets
integer x = 4791
integer y = 1704
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
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png  "
alignment htextalign = left!
end type

event clicked;Close(parent)
end event

type cb_2 from commandbutton within w_emision_adhesivos_pallets
boolean visible = false
integer x = 608
integer y = 588
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

type cb_1 from commandbutton within w_emision_adhesivos_pallets
boolean visible = false
integer x = 206
integer y = 588
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

type gb_4 from groupbox within w_emision_adhesivos_pallets
integer x = 2469
integer y = 224
integer width = 1792
integer height = 176
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type iuo_cliente from uo_seleccion_cliente_comun within w_emision_adhesivos_pallets
integer x = 1143
integer y = 280
integer height = 88
integer taborder = 90
end type

on iuo_cliente.destroy
call uo_seleccion_cliente_comun::destroy
end on

type pb_imprimir from picturebutton within w_emision_adhesivos_pallets
integer x = 4791
integer y = 768
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

type cbx_visible from checkbox within w_emision_adhesivos_pallets
boolean visible = false
integer x = 55
integer y = 56
integer width = 672
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Formato Completo"
boolean checked = true
end type

event clicked;IF THIS.Checked THEN
	THIS.Text = 'Formato Completo'
	
ELSE
	THIS.Text = 'Solo Datos'
	
END IF
end event

type st_encabe from statictext within w_emision_adhesivos_pallets
integer x = 41
integer y = 44
integer width = 4663
integer height = 384
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_emision_adhesivos_pallets
boolean visible = false
integer x = 50
integer y = 84
integer width = 498
integer height = 344
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_adhesivos_pallets"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_1 from datawindow within w_emision_adhesivos_pallets
integer x = 91
integer y = 480
integer width = 4567
integer height = 1484
integer taborder = 50
boolean titlebar = true
string title = "Pallets Seleccionados"
string dataobject = "dw_mues_seleccion_pallets_granel"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;This.SelectRow(0, False)

IF Row > 0 THEN
	This.SelectRow(Row, True)
	pb_ventanas.ii_procedencia	=	1
	pb_ventanas.ii_cliente		=	This.Object.clie_codigo[Row]
	pb_ventanas.il_planta		=	This.Object.plde_codigo[Row]
	pb_ventanas.il_pallet		=	This.Object.paen_numero[Row]
	pb_ventanas.ii_especie		=	This.Object.espe_codigo[Row]
	pb_ventanas.ii_cajas			=	This.Object.paen_ccajas[Row]
	pb_ventanas.ii_impresiones	=	Integer(em_copias.Text)
	pb_ventanas.ii_operacion	=	ii_operacion
	pb_ventanas.ii_sistema		=	ii_sistema
	pb_ventanas.ii_proceso		=	This.Object.proceso[Row]
	
	pb_ventanas.Enabled 			= 	True
	
ELSE
	pb_ventanas.Enabled 			= 	False
	
END IF
end event

type st_2 from statictext within w_emision_adhesivos_pallets
integer x = 41
integer y = 428
integer width = 4663
integer height = 1584
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

