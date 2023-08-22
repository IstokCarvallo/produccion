$PBExportHeader$w_mant_deta_distribcalibre.srw
forward
global type w_mant_deta_distribcalibre from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_distribcalibre from w_mant_detalle_csd
integer width = 2322
integer height = 988
end type
global w_mant_deta_distribcalibre w_mant_deta_distribcalibre

forward prototypes
public function boolean cargalote ()
public subroutine calculakilos (integer ai_columna, decimal ad_valor)
public function boolean activacion (integer ai_estado)
end prototypes

public function boolean cargalote ();str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1]
lstr_busq.argum[2]	=	istr_mant.argumento[2]
lstr_busq.argum[3]	=	istr_mant.argumento[3]
lstr_busq.argum[4]	=	istr_mant.argumento[4]

OpenWithParm(w_busc_lotesdisponibles_distcalib, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[5] <> '' THEN
	dw_1.Object.lote_pltcod[il_fila]	=	Long(lstr_busq.Argum[5])
	dw_1.Object.lote_espcod[il_fila]	=	Integer(lstr_busq.Argum[6])
	dw_1.Object.lote_codigo[il_fila]	=	Long(lstr_busq.Argum[7])
	dw_1.Object.ccdc_calibr[il_fila]	=	lstr_busq.Argum[8]
	dw_1.Object.dcpl_correl[il_fila]	=	1
	dw_1.Object.opvd_porcne[il_fila]	=	0
	dw_1.Object.opvd_pespor[il_fila]	=	0
	dw_1.Object.opvd_pesone[il_fila]	=	0
	
	Return True
	
END IF

Return False
end function

public subroutine calculakilos (integer ai_columna, decimal ad_valor);
Decimal	ld_porexp, ld_porcal

ld_porexp	=	dw_1.Object.ccre_porexp[il_fila]
ld_porcal	=	dw_1.Object.cidc_porcen[il_fila]

CHOOSE CASE ai_columna
	CASE 1
		ld_porexp	=	ad_valor
		
	CASE 2
		ld_porcal	=	ad_valor
		
END CHOOSE

//(opvd_pesone * ccre_porexp) / 100

dw_1.Object.opvd_pespor[il_fila] = ((dw_1.Object.opvd_pesone[il_fila] * ld_porexp) / 100) 
dw_1.Object.opvd_porcne[il_fila] = ((dw_1.Object.opvd_pespor[il_fila] * ld_porcal) / 100)
end subroutine

public function boolean activacion (integer ai_estado);Integer	li_filas
String	ls_estado

IF ai_estado = 0 THEN
	li_filas	=	dw_1.Find("lote_codigo <> " + String(dw_1.Object.lote_codigo[il_fila]) + " and " + &
								 "dcpl_correl = " + String(ai_estado), 1, dw_1.RowCount())
								 
ELSE
	li_filas	=	dw_1.Find("lote_codigo <> " + String(dw_1.Object.lote_codigo[il_fila]) + " and " +&
								 "dcpl_correl <> " + String(ai_estado), 1, dw_1.RowCount())
								 
END IF
							 
IF ai_estado = 0 AND li_filas <> 0 THEN
	li_filas = MessageBox("Error", "Ya existe un lote activo para la orden seleccionada, "+&
											 "¿Desea desactivarlo?", Question!, OkCancel!)
	IF li_filas = 1 THEN
		FOR li_filas = 1 TO dw_1.RowCount()
			IF dw_1.Object.lote_codigo[il_fila] <> dw_1.Object.lote_codigo[li_filas] THEN
				dw_1.Object.dcpl_correl[li_filas] = 1
			END IF
			
		NEXT

		FOR li_filas = 1 TO dw_1.RowCount()
			IF dw_1.Object.lote_codigo[il_fila] = dw_1.Object.lote_codigo[li_filas] THEN
				dw_1.Object.dcpl_correl[li_filas] = 0
			END IF

		NEXT
		
		Return True
		
	ELSE
		REturn False
		
	END IF
	
ELSEIF ai_estado = 0 AND li_filas = 0 THEN
	FOR li_filas = 1 TO dw_1.RowCount()
		IF dw_1.Object.lote_codigo[il_fila] = dw_1.Object.lote_codigo[li_filas] THEN
			dw_1.Object.dcpl_correl[li_filas] = 0
		END IF

	NEXT
	
	Return True
ELSEIF ai_estado = 1 AND li_filas = 0 THEN
	li_filas = MessageBox("Error", "Esta dejando la orden de proceso sin lote activo, " + &
											 "¿Esta seguro?. ~r~nLa orden usará por defecto el lote mayor.", &
											 Question!, OkCancel!)
	IF li_filas = 1 THEN
		FOR li_filas = 1 TO dw_1.RowCount()
			IF dw_1.Object.lote_codigo[il_fila] = dw_1.Object.lote_codigo[li_filas] THEN
				dw_1.Object.dcpl_correl[li_filas] = 1
			END IF
		NEXT
		
		Return True
		
	ELSE
		FOR li_filas = 1 TO dw_1.RowCount()
			IF dw_1.Object.lote_codigo[il_fila] = dw_1.Object.lote_codigo[li_filas] THEN
				dw_1.Object.dcpl_correl[li_filas] = 0
			END IF
		NEXT
		
		Return False
		
	END IF
END IF

REturn True
end function

on w_mant_deta_distribcalibre.create
call super::create
end on

on w_mant_deta_distribcalibre.destroy
call super::destroy
end on

event ue_antesguardar;call super::ue_antesguardar;Integer	li_filas 
Decimal	ld_porcent

FOR li_filas = 1 TO dw_1.RowCount()
	IF dw_1.Object.lote_codigo[il_fila] = dw_1.Object.lote_codigo[li_filas] THEN
		ld_porcent	=	ld_porcent + dw_1.Object.cidc_porcen[li_filas]
	END IF
NEXT

IF ld_porcent > 100 THEN 
	Messagebox("Error", "La suma de distribución de calibres para este lote sobrepasa el 100%", StopSign!)
	Message.DoubleParm = -1
	
END IF
end event

event ue_deshace;call super::ue_deshace;dw_1.Object.Lote_pltcod[il_fila]	=	Long(istr_mant.Argumento[5])
dw_1.Object.Lote_espcod[il_fila]	=	Integer(istr_mant.Argumento[6])
dw_1.Object.Lote_codigo[il_fila]	=	Long(istr_mant.Argumento[7])
dw_1.Object.ccdc_calibr[il_fila]	=	istr_mant.Argumento[8]
dw_1.Object.cidc_porcen[il_fila]	=	Dec(istr_mant.Argumento[9])
dw_1.Object.opvd_pespor[il_fila]	=	Dec(istr_mant.Argumento[10])
dw_1.Object.opvd_porcne[il_fila]	=	dec(istr_mant.Argumento[11])
dw_1.Object.dcpl_correl[il_fila]	=	Dec(istr_mant.Argumento[12])
end event

event ue_recuperadatos;call super::ue_recuperadatos;istr_mant.Argumento[5]	=	String(dw_1.Object.Lote_pltcod[il_fila])
istr_mant.Argumento[6]	=	String(dw_1.Object.Lote_espcod[il_fila])
istr_mant.Argumento[7]	=	String(dw_1.Object.Lote_codigo[il_fila])
istr_mant.Argumento[8]	=	String(dw_1.Object.ccdc_calibr[il_fila])
istr_mant.Argumento[9]	=	String(dw_1.Object.cidc_porcen[il_fila])
istr_mant.Argumento[10]	=	String(dw_1.Object.opvd_pespor[il_fila])
istr_mant.Argumento[11]	=	String(dw_1.Object.opvd_porcne[il_fila])
istr_mant.Argumento[12]	=	String(dw_1.Object.dcpl_correl[il_fila])
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_distribcalibre
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_distribcalibre
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_distribcalibre
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_distribcalibre
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_distribcalibre
integer x = 1902
integer y = 372
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_distribcalibre
integer x = 1902
end type

event pb_acepta::clicked;
istr_mant.respuesta = 1

Parent.TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = 0 THEN
	
	IF istr_mant.agrega THEN
		dw_1.Object.clie_codigo[il_fila]	=	Integer(istr_mant.argumento[1])
		dw_1.Object.plde_codigo[il_fila]	=	Long(istr_mant.argumento[2])
		dw_1.Object.orpr_tipord[il_fila]	=	Integer(istr_mant.argumento[3])
		dw_1.Object.orpr_numero[il_fila]	=	Long(istr_mant.argumento[4])
		
		Parent.TriggerEvent("ue_nuevo")
	ELSE
		CloseWithReturn(Parent, istr_mant)
	END IF
ELSE
	dw_1.SetColumn("cidc_porcen")
	Return
	
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_distribcalibre
integer x = 1906
integer y = 620
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_distribcalibre
integer y = 96
integer width = 1705
integer height = 740
string dataobject = "dw_mant_deta_distribcalibre"
end type

event dw_1::buttonclicked;call super::buttonclicked;String	ls_columna

ls_columna	=	dwo.name

CHOOSE CASE ls_columna
	CASE "b_lote"
		CargaLote()
		
END CHOOSE
end event

event dw_1::itemchanged;ib_modifica = True

Integer	li_null, li_valor
String	ls_columna
string 	PassedString

SetNull(li_null)

ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "ccre_porexp"
		CalculaKilos(1, Dec(data))
	
	CASE "cidc_porcen"
		CalculaKilos(2, Dec(data))
		
	CASE "dcpl_correl"
		li_valor	=	This.Object.dcpl_correl[row]
		IF NOT Activacion(Integer(data)) THEN
			IF data = '1' THEN
				This.Object.dcpl_correl[row]	=	li_valor
			ELSE
				This.Object.dcpl_correl[row]	=	li_valor
			END IF
			Return 1
		END IF
		
END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

