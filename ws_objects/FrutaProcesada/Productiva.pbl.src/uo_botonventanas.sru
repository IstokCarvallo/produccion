$PBExportHeader$uo_botonventanas.sru
forward
global type uo_botonventanas from picturebutton
end type
end forward

global type uo_botonventanas from picturebutton
integer width = 178
integer height = 156
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\ADHESIVOSE.bmp"
string disabledname = "\Desarrollo\Bmp\ADHESIVOSD.bmp"
alignment htextalign = left!
end type
global uo_botonventanas uo_botonventanas

type variables
Integer								ii_cliente, ii_especie, ii_procedencia, ii_impresiones, ii_operacion, ii_cajas, ii_sistema
Long									il_planta, il_pallet, ii_proceso=1, ii_palletdate = 0

uo_controlventanas				iuo_ctrlvent
uo_clientesprod					iuo_cliente
w_emision_adhesivos_pallets	iw_imprimir


end variables

forward prototypes
public subroutine impresion ()
public subroutine reimpresion ()
end prototypes

public subroutine impresion ();Integer	li_fila
Long 		ll_Control

ll_Control	=	Long(il_planta * ii_cliente * ii_especie * ii_procedencia)	

IF ll_control < 1 OR IsNull(ll_control) THEN
	Return 
END IF

//ai_operacion
//		1	=	Impresion
//		2	=	ReImpresion
IF ii_palletdate <> 1 THEN
	IF NOT iuo_ctrlvent.ValidaRango(ii_cliente, il_planta, il_pallet, ii_operacion, 0, ii_proceso, ii_cajas) THEN
		Return
		
	END IF
END IF

Open(iw_imprimir)

iw_imprimir.Visible			=	False
iw_imprimir.em_copias.Text	=	String(ii_impresiones)

//ii_procedencia solo para la ventana
//		0	=	Solo Codigo GS1
//		1	=	Ventana Pallet
iw_imprimir.ii_procedencia	=	0
iw_imprimir.ii_operacion	=	ii_operacion
iw_imprimir.ii_sistema		=	ii_sistema

IF ii_palletdate = 1 THEN 
	iw_imprimir.cbx_palletdate.Checked	=	True
END IF
iw_imprimir.cbx_palletdate.TriggerEvent("Clicked")

iw_imprimir.dw_1.Retrieve(il_planta, ii_cliente, ii_especie, ii_sistema, -1, ii_operacion)

li_fila	=	iw_imprimir.dw_1.RowCount()

IF li_fila < 1 THEN 
	MessageBox("Error", "No es posible desplegar Pallets en existencia", StopSign!)

ELSE
	li_fila	=	iw_imprimir.dw_1.Find("paen_numero = " + String(il_pallet), 1, iw_imprimir.dw_1.RowCount())
	IF li_fila > 0 THEN
		iw_imprimir.dw_1.SelectRow(0, False)
		iw_imprimir.dw_1.SelectRow(li_fila, True)
		iw_imprimir.pb_imprimir.TriggerEvent(Clicked!)
	ELSE
		MessageBox("Error", "No es posible imprimir ventana de pallet Solicitado", StopSign!)
	END IF
END IF

IF ii_palletdate <> 1 THEN 
	iuo_ctrlvent.ActualizaControl(ii_cliente, il_planta, il_pallet, ii_operacion, 0, ii_proceso)
END IF

Close(iw_imprimir)
end subroutine

public subroutine reimpresion ();Integer	li_fila
Long 		ll_Control

ll_Control	=	Long(il_planta * ii_cliente * ii_especie * ii_procedencia)	

IF ll_control < 1 OR IsNull(ll_control) THEN
	Return 
END IF

//ai_operacion
//		1	=	Impresion
//		2	=	ReImpresion
IF ii_palletdate <> 1 THEN
	IF NOT iuo_ctrlvent.ValidaRango(ii_cliente, il_planta, il_pallet, ii_operacion, ii_impresiones,ii_proceso, ii_cajas) THEN
		Return
	END IF
END IF

Open(iw_imprimir)

iw_imprimir.Visible			=	False
iw_imprimir.em_copias.Text	=	String(ii_impresiones)

//ii_procedencia solo para la ventana
//		0	=	Solo Codigo GS1
//		1	=	Ventana Pallet
iw_imprimir.ii_procedencia	=	0
iw_imprimir.ii_operacion	=	ii_operacion
iw_imprimir.ii_sistema		=	ii_sistema

IF ii_palletdate = 1 THEN 
	iw_imprimir.cbx_palletdate.Checked	=	True
END IF

iw_imprimir.cbx_palletdate.TriggerEvent("Clicked")
iw_imprimir.dw_1.Retrieve(il_planta, ii_cliente, ii_especie, ii_sistema, -1, ii_operacion)

li_fila	=	iw_imprimir.dw_1.RowCount()

IF li_fila < 1 THEN 
	MessageBox("Error", "No es posible desplegar Pallets en existencia", StopSign!)
ELSE
	li_fila	=	iw_imprimir.dw_1.Find("paen_numero = " + String(il_pallet), &
												 1, iw_imprimir.dw_1.RowCount())
	IF li_fila > 0 THEN
		iw_imprimir.dw_1.SelectRow(0, False)
		iw_imprimir.dw_1.SelectRow(li_fila, True)
		iw_imprimir.pb_imprimir.TriggerEvent(Clicked!)
	ELSE
		MessageBox("Error", "No es posible imprimir ventana de pallet Solicitado", StopSign!)
	END IF
END IF

IF ii_palletdate <> 1 THEN 
	iuo_ctrlvent.ActualizaControl(ii_cliente, il_planta, il_pallet, ii_operacion, ii_impresiones, ii_proceso)
END IF

Close(iw_imprimir)
end subroutine

on uo_botonventanas.create
end on

on uo_botonventanas.destroy
end on

event constructor;iuo_ctrlvent	=	Create uo_controlventanas
iuo_cliente		=	Create uo_clientesprod
end event

event destructor;Destroy iuo_ctrlvent;
end event

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

