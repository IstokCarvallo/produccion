$PBExportHeader$w_mant_deta_ctlcalplacualinspuvadet.srw
$PBExportComments$Mantenedor de Planilla Cualitativa
forward
global type w_mant_deta_ctlcalplacualinspuvadet from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_ctlcalplacualinspuvadet from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 3451
integer height = 1124
string title = "Detalle Planilla Cualitativa de Uvas"
end type
global w_mant_deta_ctlcalplacualinspuvadet w_mant_deta_ctlcalplacualinspuvadet

type variables
uo_ctlcaldanoespecie		iuo_ctlcaldanoespecie
end variables

forward prototypes
public function boolean noexistecliente (integer ai_cliente)
public function boolean duplicado (string as_valor, integer ai_tipo)
public function boolean noexisteproductor (long al_productor)
end prototypes

public function boolean noexistecliente (integer ai_cliente);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre
	FROM	dba.clientesprod
	WHERE	clie_codigo	=	:ai_Cliente ;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla clientesprod")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente (" + String(ai_Cliente, '000') + &
	 			 "), no ha sido~r" + "ingresado en tabla respectiva.~r~r" + &
				  "Ingrese o seleccione otro Código.")

	RETURN True
ELSE
//	dw_1.SetItem(il_Fila, "variedades_vari_nombre", ls_Nombre)
	RETURN False
END IF

end function

public function boolean duplicado (string as_valor, integer ai_tipo);Long     ll_fila, ll_productor
Integer	li_agronomo, li_cliente

li_agronomo		=	dw_1.Object.ccag_codigo[il_fila]
li_cliente		=	dw_1.Object.clie_codigo[il_fila]
ll_productor	=	dw_1.Object.prod_codigo[il_fila]

CHOOSE CASE ai_tipo

	CASE 1
		li_agronomo		=	Integer(as_Valor)

	CASE 2
		li_cliente		=	Integer(as_Valor)

	CASE 3
		ll_productor	=	Long(as_valor)

END CHOOSE

ll_fila = dw_1.Find("ccag_codigo = " + String(li_agronomo) + &
							" AND clie_codigo = " + String(li_cliente) + &
							" AND prod_codigo = " + String(ll_productor), + &
							1, dw_1.RowCount())

IF ll_fila > 0 AND ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public function boolean noexisteproductor (long al_productor);Integer  li_Cliente
String	ls_Productor

li_Cliente	=	Integer(dw_1.Object.clie_codigo[il_Fila])

IF IsNull(li_Cliente) = False AND IsNull(al_Productor) = False THEN
	SELECT	prod_nombre
		INTO	:ls_Productor
		FROM	dba.productores
		WHERE	clie_codigo	= :li_Cliente
		AND	prod_codigo	= :al_Productor ;

	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Productores")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de Productor (" + String(al_productor, '00000') + &
		 			 "), no ha sido~r" + "ingresado en tabla respectiva.~r~r" + &
					  "Ingrese o seleccione otro Código.")

		RETURN True
	ELSE
		dw_1.SetItem(il_fila, "prod_nombre", ls_Productor)
	END IF
END IF

RETURN False
end function

on w_mant_deta_ctlcalplacualinspuvadet.create
call super::create
end on

on w_mant_deta_ctlcalplacualinspuvadet.destroy
call super::destroy
end on

event ue_recuperadatos();call super::ue_recuperadatos;String ls_Usuario
Integer li_Grupo

ls_Usuario	=	Upper(Gstr_Us.Nombre)

IF dw_1.RowCount() > 0 THEN	
	li_Grupo	=	BuscaGrupo(ls_Usuario)
	IF ((li_grupo = 1  OR li_grupo = 6) OR dw_1.GetItemStatus(il_Fila, 0, Primary!) = NewModified!) THEN					
		ias_Campo[1]  = String(dw_1.Object.cccd_embal1[il_Fila])
		ias_Campo[2]  = String(dw_1.Object.cccd_embal2[il_Fila])
		ias_Campo[3]  = String(dw_1.Object.cccd_embal3[il_Fila])
		ias_Campo[4]  = dw_1.Object.cccd_calemb[il_Fila]
		ias_Campo[5]  = String(dw_1.Object.cccd_calid1[il_Fila])
		ias_Campo[6]  = String(dw_1.Object.cccd_calid2[il_Fila])
		ias_Campo[7]  = String(dw_1.Object.cccd_calid3[il_Fila])
		ias_Campo[8]  = dw_1.Object.cccd_calcal[il_Fila]
		ias_Campo[9]  = String(dw_1.Object.cccd_condi1[il_Fila])
		ias_Campo[10] = String(dw_1.Object.cccd_condi2[il_Fila])
		ias_Campo[11] = String(dw_1.Object.cccd_condi3[il_Fila])
		ias_Campo[12] = dw_1.Object.cccd_calcon[il_Fila]		
		dw_1.Enabled				=	True
		Pb_Acepta.Enabled			=	True 		
		Pb_Cancela.Enabled		=	True 
		Pb_Salir.Enabled 			=	True 
	ELSE			
		istr_mant.Solo_consulta	=	True 
		dw_1.Enabled				=	False			
	END IF 	
END IF 	
	



end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.SetItem(il_Fila, "cccd_embal1", Integer(ias_Campo[1]))
	dw_1.SetItem(il_Fila, "cccd_embal2", Integer(ias_Campo[2]))
	dw_1.SetItem(il_Fila, "cccd_embal3", Integer(ias_Campo[3]))
	dw_1.SetItem(il_Fila, "cccd_calemb", ias_Campo[4])
	dw_1.SetItem(il_Fila, "cccd_calid1", Integer(ias_Campo[5]))
	dw_1.SetItem(il_Fila, "cccd_calid2", Integer(ias_Campo[6]))
	dw_1.SetItem(il_Fila, "cccd_calid3", Integer(ias_Campo[7]))
	dw_1.SetItem(il_Fila, "cccd_calcal", ias_Campo[8])
	dw_1.SetItem(il_Fila, "cccd_condi1", Integer(ias_Campo[9]))
	dw_1.SetItem(il_Fila, "cccd_condi2", Integer(ias_Campo[10]))
	dw_1.SetItem(il_Fila, "cccd_condi3", Integer(ias_Campo[11]))
	dw_1.SetItem(il_Fila, "cccd_calcon", ias_Campo[12])
END IF
end event

event ue_antesguardar();String	ls_Mensaje, ls_Columna[]
Integer	li_Contador

IF Isnull(dw_1.Object.cccd_calemb[il_Fila]) OR dw_1.Object.cccd_calemb[il_Fila] = "" THEN
	li_Contador	++
	ls_Mensaje 			= ls_Mensaje + "~nCalificación Embalaje"
	ls_Columna[li_Contador]	= "cccd_calemb"
END IF

IF Isnull(dw_1.Object.cccd_calcal[il_Fila]) OR dw_1.Object.cccd_calcal[il_Fila] = "" THEN
	li_Contador	++
	ls_Mensaje 			= ls_Mensaje + "~nCalificación Calidad"
	ls_Columna[li_Contador]	= "cccd_calcal"
END IF

IF Isnull(dw_1.Object.cccd_calcon[il_Fila]) OR dw_1.Object.cccd_calcon[il_Fila] = "" THEN
	li_Contador	++
	ls_Mensaje 			= ls_Mensaje + "~nCalificación Condición"
	ls_Columna[li_Contador]	= "cccd_calcon"
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo();call super::ue_nuevo;//dw_1.SetItem(il_fila, "zona_codigo", Integer(istr_mant.argumento[1]))

end event

event open;x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono
PostEvent("ue_recuperadatos")
istr_Mant = Message.PowerObjectParm
istr_Mant.dw.ShareData(dw_1)

dw_1.SetTransObject(SqlCa)

iuo_ctlcaldanoespecie	=	Create uo_ctlcaldanoespecie
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_ctlcalplacualinspuvadet
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_ctlcalplacualinspuvadet
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_ctlcalplacualinspuvadet
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_ctlcalplacualinspuvadet
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_ctlcalplacualinspuvadet
integer x = 3177
integer y = 348
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_ctlcalplacualinspuvadet
integer x = 3173
integer y = 132
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_ctlcalplacualinspuvadet
integer x = 3173
integer y = 564
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_ctlcalplacualinspuvadet
integer y = 76
integer width = 2985
integer height = 852
string dataobject = "dw_mant_ctlcalplacualinspuvadet"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Null

SetNull(ls_Null)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	
	CASE "cccd_embal1", "cccd_embal2", "cccd_embal3"
		IF NOT iuo_ctlcaldanoespecie.Existe(Integer(istr_Mant.Argumento[7]), &
														Integer(Data), True, SqlCa) THEN
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

	CASE "cccd_calid1", "cccd_calid2", "cccd_calid3"
		IF NOT iuo_ctlcaldanoespecie.Existe(Integer(istr_Mant.Argumento[7]), &
														Integer(Data), True, SqlCa) THEN
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

	CASE "cccd_condi1", "cccd_condi2", "cccd_condi3"
		IF NOT iuo_ctlcaldanoespecie.Existe(Integer(istr_Mant.Argumento[7]), &
														Integer(Data), True, SqlCa) THEN
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			RETURN 1
		END IF

END CHOOSE
end event

