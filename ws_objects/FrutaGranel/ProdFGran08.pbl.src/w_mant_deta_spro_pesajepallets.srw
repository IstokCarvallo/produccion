$PBExportHeader$w_mant_deta_spro_pesajepallets.srw
$PBExportComments$Mantención Detalle de Bins vaciados a Proceso
forward
global type w_mant_deta_spro_pesajepallets from w_mant_detalle_csd
end type
type ole_puerta from olecustomcontrol within w_mant_deta_spro_pesajepallets
end type
type str_pesaje from structure within w_mant_deta_spro_pesajepallets
end type
end forward

type str_pesaje from structure
	datetime		fechahora[]
	decimal { 4 }		pesaje[]
	decimal { 4 }		total
	boolean		agrega
	boolean		modifica
	str_puertacomm		puerta
	datawindow		dw
	string		argum[]
end type

global type w_mant_deta_spro_pesajepallets from w_mant_detalle_csd
integer width = 2386
integer height = 1080
string title = "Ingreso de Detalle Pesaje de Pallets"
ole_puerta ole_puerta
end type
global w_mant_deta_spro_pesajepallets w_mant_deta_spro_pesajepallets

type variables

Decimal 						id_pesosbins[], id_taraenvase
Integer						ii_estadoRetrieve
DataWindowChild			idwc_envases, idwc_calicosechero
Long							il_tarja
str_mant						istr_mant2
uo_pallet_gr						iuo_pallet

Private:
str_pesaje              		wstr_pesaje
str_puertacomm		      istr_puertacomm
end variables

forward prototypes
public function boolean validapallet (long al_pallet)
public subroutine destarapallet ()
end prototypes

public function boolean validapallet (long al_pallet);Boolean	lb_retorno = TRUE
integer	li_cantidad

select count(*)
  into :li_cantidad
  from dbo.spro_pesajepallets
  where paen_numero = :al_pallet;

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de correlativos de compactos: spro_pesajepallets")
	RETURN FALSE
ELSEIF li_cantidad > 0 THEN
	MessageBox("Error","El pallet  " + String(al_pallet) + " ya ha sido ingresado")
	RETURN FALSE
END IF


Return lb_retorno
end function

public subroutine destarapallet ();IF il_fila > 0 THEN
	dw_1.Object.paen_kilbru[il_fila]	=	dw_1.Object.kilos[il_fila]
END IF
end subroutine

on w_mant_deta_spro_pesajepallets.create
int iCurrent
call super::create
this.ole_puerta=create ole_puerta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.ole_puerta
end on

on w_mant_deta_spro_pesajepallets.destroy
call super::destroy
destroy(this.ole_puerta)
end on

event open;call super::open;Integer	li_resultado
String		ls_parametros
Boolean	ib_OCX	=	True

istr_mant2		=	Message.PowerObjectParm

iuo_pallet		=	Create uo_pallet_gr

str_pesaje			lstr_pesaje
str_puertacomm 	lstr_puertacomm

wstr_pesaje		=	lstr_pesaje

li_resultado 		=	ConfiguracionPuerta(istr_puertacomm)

IF li_resultado 	= 	0 THEN
	ls_parametros	=	String(istr_puertacomm.Baudios)+","+ &
							istr_puertacomm.Paridad      + "," + &
							String(istr_puertacomm.Data) + "," + &
							String(istr_puertacomm.Parada)
			
	IF NOT ib_OCX THEN
		MessageBox("Conexión Romana","No está instalado el OCX para conexión con Romana")
	ELSE
		IF Ole_puerta.object.PortOpen THEN Ole_puerta.object.PortOpen = False
		Ole_puerta.object.settings	=	ls_parametros
		Ole_puerta.object.PortOpen	= True	
	END IF
END IF

Timer(0.2)

end event

event ue_recuperadatos;call super::ue_recuperadatos;IF istr_mant.agrega = False and istr_mant.borra = False THEN
	ias_campo[1]  = String(dw_1.GetItemNumber(il_fila,"paen_numero"))
	ias_campo[4]  = String(dw_1.GetItemNumber(il_fila,"paen_kilnet"))
	ias_campo[5]  = String(dw_1.GetItemNumber(il_fila,"paen_kilbru"))
END IF
end event

event timer;call super::timer;//Integer	li_factor, li_posini, li_LarBuf
//String 	ls_string
//Double	ld_kilos
//
//Ole_Puerta.Object.inputlen =	Integer(istr_puertacomm.LargoLectura)
//
//li_LarBuf =Ole_Puerta.Object.InBufferCount
//
//IF li_LarBuf > 0 THEN
//	ls_string =  Ole_Puerta.Object.input
//END IF
//
//li_posini = Pos(ls_string,istr_puertacomm.CadenaInicio) + Len(istr_puertacomm.CadenaInicio)
//
//IF Len(Mid(ls_string,li_posini)) < istr_puertacomm.LargoCadena THEN RETURN
//	
//IF IsNumber(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena)) THEN
//	ld_kilos	=	Dec(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena))
//	IF istr_puertacomm.Decimales > 0 THEN
//		li_factor	= 10 ^ istr_puertacomm.Decimales
//		ld_kilos		= Round(ld_kilos/li_factor,istr_puertacomm.Decimales)
//	END IF
//	dw_1.Object.kilos[il_fila]	=	ld_kilos
//END IF
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.Object.paen_numero[il_Fila]	=	Long(ias_campo[1])
	dw_1.Object.paen_kilnet[il_Fila]	=	Long(ias_campo[4])
	dw_1.Object.paen_kilbru[il_Fila]	=	Long(ias_campo[5])
END IF
end event

event ue_guardar;call super::ue_guardar;IF dw_1.Object.paen_kilbru[il_fila] <= 0 THEN
	MessageBox("Error", "Los pesos no han sido Ingresados o asignados", StopSign!)
	Message.DoubleParm = -1
END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_spro_pesajepallets
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_spro_pesajepallets
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_spro_pesajepallets
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_spro_pesajepallets
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_spro_pesajepallets
integer x = 1966
integer y = 468
integer taborder = 70
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_spro_pesajepallets
integer x = 1966
integer y = 288
integer taborder = 60
end type

event pb_acepta::clicked;//PesoOriginalPallet()

Call Super :: clicked
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_spro_pesajepallets
integer x = 1966
integer y = 648
integer taborder = 80
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_spro_pesajepallets
integer x = 78
integer y = 124
integer width = 1778
integer height = 696
integer taborder = 30
string title = "Control Pesaje Detalle"
string dataobject = "dw_mant_mues_deta_pesajepallets"
end type

event dw_1::buttonclicked;call super::buttonclicked;string ls_columna

ls_columna = dwo.Name

CHOOSE CASE ls_columna

	CASE "botonromana"		
		IF (istr_puertacomm.pesajebins = 1 AND &
			This.Object.kilos[il_fila] >= istr_puertacomm.PesoMinimo) OR &
			(This.Object.kilos[il_fila] >= 0) THEN
				This.AcceptText()
				DestaraPallet()
		END IF
		
END CHOOSE		
end event

event dw_1::itemchanged;call super::itemchanged;Long		ll_null
Integer	li_codigo, li_null, li_bultos, li_fila
String		ls_columna
Date     	ld_fecha

SetNull(ll_null)
SetNull(li_null)

ls_columna 	=	dwo.Name

CHOOSE CASE ls_columna
	CASE "paen_numero"
		
			IF Not IsNull(data) THEN
				IF NOT Validapallet(Long(Data)) THEN
					This.SetItem(row, ls_Columna, li_Null)
					Return 1
				ELSE
					il_tarja	=	Long(Data)
				END IF	
			END IF
			
	CASE "orpr_tipord"	
		istr_mant.argumento[2] = Data
		dw_1.SetItem(1,"orpr_numero",ll_Null)
				
END CHOOSE
end event

type ole_puerta from olecustomcontrol within w_mant_deta_spro_pesajepallets
event oncomm ( )
boolean visible = false
integer x = 1957
integer y = 88
integer width = 174
integer height = 152
integer taborder = 90
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_mant_deta_spro_pesajepallets.win"
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
09w_mant_deta_spro_pesajepallets.bin 
2B00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
19w_mant_deta_spro_pesajepallets.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
