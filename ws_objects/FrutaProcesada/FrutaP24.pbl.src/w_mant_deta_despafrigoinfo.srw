$PBExportHeader$w_mant_deta_despafrigoinfo.srw
forward
global type w_mant_deta_despafrigoinfo from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_despafrigoinfo from w_mant_detalle_csd
integer width = 3739
integer height = 1276
end type
global w_mant_deta_despafrigoinfo w_mant_deta_despafrigoinfo

type variables
Integer ii_estado
end variables

forward prototypes
public function boolean noexisteinspeccion (long numero)
public function boolean duplicado (long al_numero)
public function boolean existepallet (long al_numero)
public function boolean existeespecie (integer ai_especie)
end prototypes

public function boolean noexisteinspeccion (long numero);Long		ll_cont
Integer	li_cliente, li_planta, li_especie
Date		ld_fecharevision

li_cliente	= 	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])
li_especie  =	Integer(istr_mant.argumento[10])

SELECT	count(*)
	INTO	:ll_cont
	FROM	dbo.fumigaenc
	WHERE clie_codigo	= 	:li_cliente
	AND	plde_codigo	=	:li_planta
	AND   cond_codigo =  2
   AND   fumi_numsag	=	:numero;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla fumigaenc")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención", "Número de Lote NO Existe, Ingrese otro Número.", &
			Exclamation!, OK!)
	RETURN True
ELSE
	SELECT	distinct fumi_fecfum
	INTO	:ld_fecharevision
	FROM	dbo.fumigaenc
	WHERE clie_codigo	= 	:li_cliente
	AND	plde_codigo	=	:li_planta
	AND   cond_codigo =  2
   AND   fumi_numsag	=	:numero;
	
	dw_1.Object.defi_fechmu[il_fila]	=	ld_fecharevision
	
END IF

RETURN False
end function

public function boolean duplicado (long al_numero);Long		ll_fila, ll_inspeccion
Integer	li_cliente, li_planta, li_cantid

ll_inspeccion = al_numero

ll_fila	=	dw_1.Find("clie_codigo = " + istr_mant.Argumento[3] + &
						" AND plde_codigo = " + istr_mant.Argumento[1] + &
						" AND defi_nrlote = " + String(ll_inspeccion) , 1, dw_1.RowCount())
						//" AND defi_pallet = " + String(al_Numero) + &
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Lote Ya está Registrado", Information!, Ok!)
	RETURN True
END IF


RETURN False
end function

public function boolean existepallet (long al_numero);Long		ll_cont,ll_inspección
Integer	li_cliente, li_planta, li_especie

li_cliente		= 	Integer(istr_mant.argumento[3])
li_planta		=	Integer(istr_mant.argumento[1])
li_especie  	=	Integer(istr_mant.argumento[10])
ll_inspección	=	Long(dw_1.Object.defi_nrlote[il_fila])

SELECT	count(*)
	INTO	:ll_cont
	FROM	dbo.inspecpaldet as det,dbo.palletencab as pan
	WHERE det.clie_codigo	= 	:li_cliente
	AND	det.plde_codigo	=	:li_planta
   AND   det.inpe_numero	=	:ll_inspección
	AND	det.paen_numero 	=  :al_numero
	AND	det.clie_codigo 	=	pan.clie_codigo
	AND	det.plde_codigo 	=	pan.plde_codigo
	AND	det.paen_numero	=	pan.paen_numero
	AND	pan.paen_estado 	=	1;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla inspecpaldet")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención", "Número de Pallet NO Existe Para Esa Inspección, Ingrese otro Número.", &
			Exclamation!, OK!)
	RETURN True
END IF

RETURN False
end function

public function boolean existeespecie (integer ai_especie);Long		ll_cont,ll_inspección
Integer	li_cliente, li_planta, li_especie

SELECT	count(*)
	INTO	:ll_cont
	FROM	dbo.especies
	WHERE espe_codigo = :ai_especie;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla especies")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención", "Código de especie no existe, Ingrese otro Código.", &
			Exclamation!, OK!)
	RETURN True
END IF

RETURN False
end function

on w_mant_deta_despafrigoinfo.create
call super::create
end on

on w_mant_deta_despafrigoinfo.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;dw_1.modify("buscapallet.Visible=1")

ias_campo[1]	=	String(dw_1.Object.clie_codigo[il_fila])
ias_campo[2]	=	String(dw_1.Object.plde_codigo[il_fila])

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "defe_numero", Long(istr_mant.argumento[2]))
END IF

end event

event ue_deshace;call super::ue_deshace;dw_1.Object.plde_codigo[il_fila]	=	Integer(ias_campo[1])
dw_1.Object.clie_codigo[il_fila]	=	Integer(ias_campo[2])
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.defi_nrlote[il_fila]) OR dw_1.Object.defi_nrlote[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFalta Número de Lote"
	ls_colu[li_cont]	= "defi_nrlote"
END IF

IF Isnull(dw_1.Object.defi_cantid[il_fila]) OR dw_1.Object.defi_cantid[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFalta Cantidad Cajas"
	ls_colu[li_cont]	= "defi_cantid"
END IF

IF Isnull(dw_1.Object.defi_tkilos[il_fila]) OR dw_1.Object.defi_tkilos[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nFalta Kilos Totales"
	ls_colu[li_cont]	= "defi_tkilos"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus() 
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;ib_ok = True

This.TriggerEvent("ue_guardar")
IF Message.DoubleParm = -1 THEN ib_ok = False

dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "defe_numero", Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))

dw_1.SetColumn("defi_nrlote")
dw_1.SetFocus()

end event

event open;x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN
	Message.DoubleParm = -1 
	RETURN
END IF

SetPointer(HourGlass!)

Message.DoubleParm = 0

w_main.SetMicroHelp("Grabando información...")
//TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_despafrigoinfo
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_despafrigoinfo
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_despafrigoinfo
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_despafrigoinfo
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_despafrigoinfo
integer x = 3301
integer y = 616
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_despafrigoinfo
integer x = 3301
integer y = 400
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF Integer(dw_1.Object.totalcajas[1]) = Integer(istr_mant.argumento[8])  THEN
	MessageBox("Atención", "Cantidad de Cajas Completa.", &
	Exclamation!, OK!)
	CloseWithReturn(Parent, istr_mant)
	Return
END IF

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF

	




end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_despafrigoinfo
integer x = 3301
integer y = 832
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_despafrigoinfo
integer x = 82
integer y = 112
integer width = 3086
integer height = 1064
string dataobject = "dw_mant_despafrigoinfo"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_Nula
Integer	li_cliente,pallet,li_status,li_tipo

SetNull(ls_Nula)

ls_columna = dwo.name

CHOOSE CASE ls_columna
			
	CASE "defi_nrlote"
		IF Duplicado(Long(Data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			RETURN 1
		ELSEIF NoExisteInspeccion(Long(data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			RETURN 1
		END IF
		
	CASE "defi_cantid"	
		dw_1.GroupCalc()
		IF istr_mant.agrega THEN
			IF (Integer(dw_1.Object.totalcajas[1]) + long(data)) >Integer(istr_mant.argumento[8])  THEN
				MessageBox("Atención", "Cantidad de Cajas Supera El Máximo Ingresado, Ingrese Otra Cantidad.", &
				Exclamation!, OK!)
				dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
				RETURN 1
			ELSEIF Integer(dw_1.Object.totalcajas[1]) = Integer(istr_mant.argumento[8]) THEN
				MessageBox("Atención", "Cantidad de Cajas Completa.", &
				Exclamation!, OK!)
				dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
				RETURN 1
			END IF	
		END IF
		
		CASE "espe_codigo"
			IF existeespecie(Integer(data)) THEN
				dw_1.SetItem(il_fila, ls_columna, integer(ls_Nula))
				RETURN 1
			END IF
	
//	CASE "defi_pallet"
//		IF Existepallet(Long(data)) OR duplicado(Long(Data)) THEN
//			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
//			RETURN 1
//		END IF

END CHOOSE
end event

