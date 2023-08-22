$PBExportHeader$w_mant_ctlcalplanillauvas_resolucionlot.srw
forward
global type w_mant_ctlcalplanillauvas_resolucionlot from w_mant_detalle
end type
end forward

global type w_mant_ctlcalplanillauvas_resolucionlot from w_mant_detalle
integer x = 123
integer y = 96
integer width = 3406
integer height = 1040
string title = "Resolución"
end type
global w_mant_ctlcalplanillauvas_resolucionlot w_mant_ctlcalplanillauvas_resolucionlot

type variables
uo_ctlcaldanoespecie			iuo_ctlcaldanoespecie
uo_loteobjetadopendiente	iuo_Objetados
Integer ii_objetados
end variables

forward prototypes
public function boolean noexistecliente (integer ai_cliente)
public subroutine wf_nuevo ()
public function boolean duplicado (string as_valor, integer ai_tipo)
public function boolean noexisteproductor (long al_productor)
public subroutine limpiacausales ()
public function boolean causales (integer ai_causa)
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
	RETURN False
END IF

end function

public subroutine wf_nuevo ();//il_fila = dw_1.InsertRow(0)

dw_1.SetRedraw(False)
dw_1.SetRow(il_fila)
dw_1.ScrollToRow(il_fila)
//istr_mant.dw.SetRow(il_fila)
//istr_mant.dw.ScrolltoRow(il_fila)
//istr_mant.dw.SelectRow(0,False)
//istr_mant.dw.SelectRow(il_fila,True)
dw_1.SetRedraw(True)
end subroutine

public function boolean duplicado (string as_valor, integer ai_tipo);Long     ll_fila
Integer	li_agronomo, li_cliente
Long     ll_productor   

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

IF IsNull(al_Productor) = False THEN
	SELECT	prod_nombre
		INTO	:ls_Productor
		FROM	dba.productores
		WHERE	prod_codigo	= :al_Productor ;

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

public subroutine limpiacausales ();String	ls_Null
SetNull(ls_Null)

IF dw_1.Object.ccpe_caloca[1] = "A" AND &
	dw_1.Object.ccpe_caloco[1] = "A" AND &
	dw_1.Object.ccpe_caloem[1] = "A" THEN
	dw_1.Object.ccpe_causal[1]	=	Integer(ls_Null)
	dw_1.Object.ccpe_causa2[1]	=	Integer(ls_Null)
	dw_1.Object.ccpe_causa3[1]	=	Integer(ls_Null)
	dw_1.Object.ccpe_pocau1[1]	=	Dec(ls_Null)
	dw_1.Object.ccpe_pocau2[1]	=	Dec(ls_Null)
	dw_1.Object.ccpe_pocau3[1]	=	Dec(ls_Null)
END IF
end subroutine

public function boolean causales (integer ai_causa);Integer	li_Causal, li_Existe 
Boolean	lb_Retorno	
li_Causal	=	ai_causa

SELECT Count(*)
INTO	 :li_Existe 
FROM	 dba.ctlcaldanoespecie
WHERE	 espe_codigo	=	11
AND    ccda_secuen	=	:li_Causal;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
ELSEIF li_Existe	>	0	THEN
	lb_Retorno	=	True
ELSE
	MessageBox("Atención","Causal digitada, no se ha creado ",StopSign!, Ok!)
END IF
RETURN lb_Retorno


end function

on w_mant_ctlcalplanillauvas_resolucionlot.create
call super::create
end on

on w_mant_ctlcalplanillauvas_resolucionlot.destroy
call super::destroy
end on

event ue_recuperadatos;String ls_Null

SetNull(ls_Null)

w_main.SetMicroHelp("Recuperando Datos...")

SetPointer(HourGlass!)
PostEvent("ue_listo")

IF istr_mant.Agrega THEN
	pb_Cancela.Enabled	=	False
	pb_Primero.Enabled	=	False
	pb_Anterior.Enabled	=	False
	pb_Siguiente.Enabled	=	False
	pb_Ultimo.Enabled		=	False
	wf_nuevo()
ELSE
	IF dw_1.RowCount() > 1 THEN
		pb_Primero.Enabled	=	True
		pb_Anterior.Enabled	=	True
		pb_Siguiente.Enabled	=	True
		pb_Ultimo.Enabled		=	True
	END IF

	il_fila	=	istr_mant.dw2.GetRow()

	dw_1.SetRedraw(False)
	dw_1.SetRow(il_fila)
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRedraw(True)

	IF istr_mant.Borra THEN
		dw_1.Enabled		=	False
		pb_Salir.Enabled	=	False
	ELSEIF istr_mant.Solo_Consulta THEN
		dw_1.Enabled			=	False
		pb_Acepta.Enabled		=	False
		pb_Cancela.Enabled	=	False
		Pb_Salir.Enabled	=	True
	ELSE
		//pb_Salir.Enabled	=	False
	END IF
END IF

IF Not istr_mant.Agrega	THEN
	ias_Campo[1]   = dw_1.Object.ccpe_reslot[1]
	ias_Campo[2]   = dw_1.Object.ccpe_caloem[1]
	ias_Campo[3]   = dw_1.Object.ccpe_caloca[1]
	ias_Campo[4]   = dw_1.Object.ccpe_caloco[1]
	ias_Campo[5]   = String(dw_1.Object.ccpe_causal[1])
	ias_campo[6]   = String(dw_1.Object.ccpe_causa2[1])
	ias_campo[7]   = String(dw_1.Object.ccpe_causa3[1])
	ias_Campo[8]   = dw_1.Object.ccpe_respri[1]
	ias_campo[9]   = String(dw_1.Object.ccpe_pocau1[1])
	ias_campo[10]  = String(dw_1.Object.ccpe_pocau2[1])
	ias_campo[11]  = String(dw_1.Object.ccpe_pocau3[1])
END IF

IF NOT IsNull(dw_1.Object.ccpe_reslot[1]) THEN
	IF dw_1.Object.ccpe_reslot[1] = "O" THEN
		dw_1.Object.ccpe_causal.Protect	=	0
		dw_1.Object.ccpe_causal.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpe_causa2.Protect	=	0
		dw_1.Object.ccpe_causa2.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpe_causa3.Protect	=	0
		dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpe_respri.Protect	=	0
		dw_1.Object.ccpe_respri.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpe_pocau1.Protect	=	0
		dw_1.Object.ccpe_pocau1.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpe_pocau2.Protect	=	0
		dw_1.Object.ccpe_pocau2.BackGround.Color	=	RGB(255,255,255)
		dw_1.Object.ccpe_pocau3.Protect	=	0
		dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(255,255,255)
	ELSE
		dw_1.Object.ccpe_causal[1]	=	Integer(ls_Null)
		dw_1.Object.ccpe_causa2[1]	=	Integer(ls_Null)
		dw_1.Object.ccpe_causa3[1]	=	Integer(ls_Null)
		dw_1.Object.ccpe_pocau1[1]	=	Dec(ls_Null)
		dw_1.Object.ccpe_pocau2[1]	=	Dec(ls_Null)
		dw_1.Object.ccpe_pocau3[1]	=	Dec(ls_Null)
		dw_1.Object.ccpe_causal.Protect	=	1
		dw_1.Object.ccpe_causal.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpe_causa2.Protect	=	1
		dw_1.Object.ccpe_causa2.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpe_causa3.Protect	=	1
		dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpe_respri.Protect	=	1
		dw_1.Object.ccpe_respri.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpe_pocau1.Protect	=	1
		dw_1.Object.ccpe_pocau1.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpe_pocau2.Protect	=	1
		dw_1.Object.ccpe_pocau2.BackGround.Color	=	RGB(192,192,192)
		dw_1.Object.ccpe_pocau3.Protect	=	1
		dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(192,192,192)
	END IF
END IF
end event

event ue_deshace;call super::ue_deshace;String	ls_Nula

SetNull(ls_Nula)
IF UpperBound(ias_campo) > 0 THEN
	dw_1.Object.ccpe_reslot[1] = ias_campo[1]
	dw_1.Object.ccpe_caloem[1] = ias_campo[2]
	dw_1.Object.ccpe_caloca[1] = ias_campo[3]
	dw_1.Object.ccpe_caloco[1] = ias_campo[4]
	dw_1.Object.ccpe_causal[1] = Integer(ias_campo[5])
	dw_1.Object.ccpe_causa2[1] = Integer(ias_campo[6])
	dw_1.Object.ccpe_causa3[1] = Integer(ias_campo[7])
	dw_1.Object.ccpe_respri[1] = ias_campo[8]
	dw_1.Object.ccpe_pocau1[1] = Dec(ias_campo[9])
	dw_1.Object.ccpe_pocau2[1] = Dec(ias_campo[10])
	dw_1.Object.ccpe_pocau3[1] = Dec(ias_campo[11])
END IF

end event

event ue_antesguardar;String	ls_Mensaje, ls_Columna[]
Integer	li_Contador
//
//IF Isnull(dw_1.Object.ccpe_reslot[1]) OR dw_1.Object.ccpe_reslot[1] = "" THEN
//	li_Contador	++
//	ls_Mensaje 	= ls_Mensaje + "~nResolución Lote"
//	ls_Columna[li_Contador]	= "ccpe_reslot"
//END IF

//IF Isnull(dw_1.Object.ccpe_reslot[1])	= False AND dw_1.Object.ccpe_reslot[1] = "O" THEN
//	IF Isnull(dw_1.Object.ccpe_causal[1]) OR dw_1.Object.ccpe_causal[1] = 0 THEN
//		li_Contador	++
//		ls_Mensaje	= ls_Mensaje + "~nCausal"
//		ls_Columna[li_Contador]	= "ccpe_causal"
//	END IF
//END IF

//IF Isnull(dw_1.Object.ccpe_reslot[1])	= False AND dw_1.Object.ccpe_reslot[1] = "O" THEN
//	IF Isnull(dw_1.Object.ccpe_pocau1[1]) OR dw_1.Object.ccpe_pocau1[1] = 0 THEN
//		li_Contador	++
//		ls_Mensaje	= ls_Mensaje + "~nPorcentaje Causal"
//		ls_Columna[li_Contador]	= "ccpe_pocau1"
//	END IF
//END IF
//
//IF dw_1.Object.ccpe_reslot[1]="O" THEN		
//   IF  dw_1.Object.ccpe_respri[1]=	"" OR IsNull(dw_1.Object.ccpe_respri[1])  THEN		
//	    li_Contador	++
//	    ls_Mensaje		= ls_Mensaje + "~nResolución Primera Instancia"
//	    ls_Columna[li_Contador]	= "ccpe_respri"
//   END IF
//END IF
//		
//IF  dw_1.Object.ccpe_Caloem[1]=	"" OR IsNull(dw_1.Object.ccpe_caloem[1])  THEN		
//	 li_Contador	++
//	 ls_Mensaje		= ls_Mensaje + "~nCalificación Lotes Embalajes"
//	 ls_Columna[li_Contador]	= "ccpe_caloem"
//END IF	
//
//IF  dw_1.Object.ccpe_Caloco[1]=	"" OR IsNull(dw_1.Object.ccpe_caloco[1])  THEN		
//	 li_Contador	++
//	 ls_Mensaje		= ls_Mensaje + "~nCalificación Lotes Condición"
//	 ls_Columna[li_Contador]	= "ccpe_caloco"
//END IF	
//		
//IF  dw_1.Object.ccpe_Caloca[1]=	"" OR IsNull(dw_1.Object.ccpe_caloca[1])  THEN		
//	 li_Contador	++
//	 ls_Mensaje		= ls_Mensaje + "~nCalificación Lotes Calidad"
//	 ls_Columna[li_Contador]	= "ccpe_caloca"
//END IF	
		
IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el Ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event open;x	= 100
y	= 450
/*
Argumentos
23: Resolución Lote
24: Resolución Primera Instancia
*/
This.Icon	=	Gstr_apl.Icono

//dw_mant_ctlcalplanillauvas_resolucionlot   -- Detalle
//dw_mant_ctlcalplanillauvas_resollotpallet   -- Porcentaje Pallet 

PostEvent("ue_recuperadatos")
istr_Mant = Message.PowerObjectParm
istr_Mant.dw2.ShareData(dw_1)

dw_1.SetTransObject(SqlCa)

iuo_ctlcaldanoespecie	=	Create uo_ctlcaldanoespecie
iuo_Objetados			=	Create uo_loteobjetadopendiente

dw_1.Object.ccpe_causal.Protect	=	1
dw_1.Object.ccpe_causal.BackGround.Color	=	553648127
dw_1.Object.ccpe_causa2.Protect	=	1
dw_1.Object.ccpe_causa2.BackGround.Color	=	553648127
dw_1.Object.ccpe_causa3.Protect	=	1
dw_1.Object.ccpe_causa3.BackGround.Color	=	553648127
dw_1.Object.ccpe_respri.Protect	=	1
dw_1.Object.ccpe_respri.BackGround.Color	=	553648127
dw_1.Object.ccpe_pocau1.Protect	=	1
dw_1.Object.ccpe_pocau1.BackGround.Color	=	553648127
dw_1.Object.ccpe_pocau2.Protect	=	1
dw_1.Object.ccpe_pocau2.BackGround.Color	=	553648127
dw_1.Object.ccpe_pocau3.Protect	=	1
dw_1.Object.ccpe_pocau3.BackGround.Color	=	553648127

//dw_1.Object.ccpe_porapr.Protect	=	1
//dw_1.Object.ccpe_porapr.BackGround.Color	=	553648127

istr_mant.Argumento[23]	=	""
istr_mant.Argumento[24]	=	""

dw_1.Object.ccpe_reslot.Protect	=	1
/*
resolucion primera instancia se deshabilita solo si estan las 3 calificaciones aprobadas
*/

IF dw_1.Object.ccpe_caloco[1]="M" OR dw_1.Object.ccpe_caloem[1]="M" OR dw_1.Object.ccpe_caloca[1]="M"THEN
	dw_1.Object.ccpe_reslot[1]="O"
	ii_objetados=1
	dw_1.Object.ccpe_reslot.Protect	=	1
ELSE
	dw_1.Object.ccpe_reslot[1]="A"
	dw_1.Object.ccpe_reslot.Protect	=	0
	ii_objetados=0
END IF		 
IF dw_1.Object.ccpe_reslot[1]="A" THEN
	//ii_objetados=0
   dw_1.Object.ccpe_respri[1]=""
	dw_1.Object.ccpe_respri.Protect	=	0
END IF	
					 
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_ctlcalplanillauvas_resolucionlot
boolean visible = false
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_ctlcalplanillauvas_resolucionlot
boolean visible = false
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_ctlcalplanillauvas_resolucionlot
boolean visible = false
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_ctlcalplanillauvas_resolucionlot
boolean visible = false
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_ctlcalplanillauvas_resolucionlot
integer x = 3049
integer y = 328
boolean enabled = false
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_ctlcalplanillauvas_resolucionlot
integer x = 3049
integer y = 148
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;
IF ii_objetados= 1 THEN
//	IF Isnull(dw_1.Object.ccpe_pocau1[1]) OR dw_1.Object.ccpe_pocau1[1] = 0  OR &
//		Isnull(dw_1.Object.ccpe_causal[1]) OR dw_1.Object.ccpe_causal[1] = 0  THEN
//		  MessageBox("Error","Debe Ingresar Caulsales de Objeción")
//	ELSE	
	  	Message.DoubleParm	=	0
	 	Parent.TriggerEvent("ue_antesguardar")
		
		IF Message.DoubleParm = -1 THEN RETURN
		
		CloseWithReturn(Parent, istr_mant)
//	END IF
				
ELSE	
	Message.DoubleParm	=	0
	Parent.TriggerEvent("ue_antesguardar")
			
	IF Message.DoubleParm = -1 THEN RETURN
		
	CloseWithReturn(Parent, istr_mant)			
END IF
end event

type pb_salir from w_mant_detalle`pb_salir within w_mant_ctlcalplanillauvas_resolucionlot
integer x = 3049
integer y = 504
end type

event pb_salir::clicked;//
istr_mant.respuesta = 2

CloseWithReturn(Parent, istr_mant)
end event

type dw_1 from w_mant_detalle`dw_1 within w_mant_ctlcalplanillauvas_resolucionlot
integer width = 2789
integer height = 840
string dataobject = "dw_mant_ctlcalplanillauvas_resolucionlot"
end type

event dw_1::itemchanged;String	ls_Columna, ls_Null

SetNull(ls_Null)

ls_Columna = dwo.Name

dw_1.AcceptText()

Choose Case ls_Columna
	Case "ccpe_reslot"
		If iuo_Objetados.Existe(This.Object.clie_codigo[1], This.Object.plde_codigo[1], &
									This.Object.cclo_numero[1], This.Object.ccpe_numero[1], False, SQLCA) Then 
			MessageBox('Error', 'No se puede Modificar la planilla:' + String (This.Object.ccpe_numero[1], '00000000') + &
						',~r~npor que tiene Lotes Objetados Pendiente.~r~nResolucion:' + String (iuo_Objetados.ccte_numero, '00000000'), StopSign!, OK!)
						
			This.SetItem(Row, ls_Columna, This.Object.ccpe_reslot[Row])
			Return 1
		Else
			istr_mant.Argumento[23]	=	Data
			If Data	=	"O" Then
				ii_objetados=1
	
				If IsNull(dw_1.Object.ccpe_respri[1]) OR dw_1.Object.ccpe_respri[1]= "A" Then
					dw_1.Object.ccpv_respri[1]	=	'O'
				End If	
	
				dw_1.Object.ccpe_causal.Protect	=	0
				dw_1.Object.ccpe_causal.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_causa2.Protect	=	0
				dw_1.Object.ccpe_causa2.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_causa3.Protect	=	0
				dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_respri.Protect	=	0
				dw_1.Object.ccpe_respri.BackGround.Color	=	RGB(255,255,255)
				
				dw_1.Object.ccpe_pocau1.Protect	=	0
				dw_1.Object.ccpe_pocau1.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_pocau2.Protect	=	0
				dw_1.Object.ccpe_pocau2.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_pocau3.Protect	=	0
				dw_1.Object.ccpe_pocau3.BackGround.Color	=	RGB(255,255,255)
	
			ELSE
				ii_objetados=0
				dw_1.Object.ccpe_causal[1]	=	Integer(ls_Null)
				dw_1.Object.ccpe_causa2[1]	=	Integer(ls_Null)
				dw_1.Object.ccpe_causa3[1]	=	Integer(ls_Null)			
				dw_1.Object.ccpe_pocau1[1]	=	Dec(ls_Null)
				dw_1.Object.ccpe_pocau2[1]	=	Dec(ls_Null)
				dw_1.Object.ccpe_pocau3[1]	=	Dec(ls_Null)			
				dw_1.Object.ccpe_respri[1]	=	ls_Null
				dw_1.Object.ccpe_causal.Protect	=	1
				dw_1.Object.ccpe_causal.BackGround.Color	=	RGB(192,192,192)
				dw_1.Object.ccpe_causa2.Protect	=	1
				dw_1.Object.ccpe_causa2.BackGround.Color	=	RGB(192,192,192)
				dw_1.Object.ccpe_causa3.Protect	=	1
				dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(192,192,192)			
				dw_1.Object.ccpe_pocau1.Protect	=	1
				dw_1.Object.ccpe_pocau1.BackGround.Color	=	RGB(192,192,192)
				dw_1.Object.ccpe_pocau2.Protect	=	1
				dw_1.Object.ccpe_pocau2.BackGround.Color	=	RGB(192,192,192)
				dw_1.Object.ccpe_pocau3.Protect	=	1
				dw_1.Object.ccpe_pocau3.BackGround.Color	=	RGB(192,192,192)
				dw_1.Object.ccpe_respri.Protect	=	1
				dw_1.Object.ccpe_respri.BackGround.Color	=	RGB(192,192,192)			
			End If
		End If

	Case	"ccpe_causal"		
		If NOT Causales(Integer(data)) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1 
		End If 				
		
	Case	"ccpe_causa2"		
		If NOT Causales(Integer(data)) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1 
		End If 		
	
	Case	"ccpe_causa3"
		If NOT Causales(Integer(data)) Then
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1 
		End If 		
		
	Case "ccpe_respri"
		If iuo_Objetados.Existe(This.Object.clie_codigo[1], This.Object.plde_codigo[1], &
									This.Object.cclo_numero[1], This.Object.ccpe_numero[1], False, SQLCA) Then 
			MessageBox('Error', 'No se puede Modificar la planilla:' + String (This.Object.ccpe_numero[1], '00000000') + &
						',~r~npor que tiene Lotes Objetados Pendiente.~r~nResolucion:' + String (iuo_Objetados.ccte_numero, '00000000'), StopSign!, OK!)
						
			This.SetItem(Row, ls_Columna, This.Object.ccpe_respri[Row])
			Return 1
		Else
			istr_mant.Argumento[24]	=	Data
		End If
		
	Case	"ccpe_pocau1","ccpe_pocau2","ccpe_pocau3"		
		If Dec(data) > 100 Then
			This.SetItem(1, ls_Columna, Dec(ls_Null))
			RETURN 1 
		End If 		
		
	Case	"ccpe_caloem"	
		If data="M" Then
			dw_1.Object.ccpe_reslot[1]="O"
			dw_1.Object.ccpe_respri[1]="O"

			ii_objetados=1
			dw_1.Object.ccpe_causal.Protect	=	0
			dw_1.Object.ccpe_causal.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_causa2.Protect	=	0
			dw_1.Object.ccpe_causa2.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_causa3.Protect	=	0
			dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_respri.Protect	=	0
			dw_1.Object.ccpe_respri.BackGround.Color	=	RGB(255,255,255)
			
			dw_1.Object.ccpe_pocau1.Protect	=	0
			dw_1.Object.ccpe_pocau1.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_pocau2.Protect	=	0
			dw_1.Object.ccpe_pocau2.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_pocau3.Protect	=	0
			dw_1.Object.ccpe_pocau3.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_reslot.Protect	=	1
		ELSE
			If dw_1.Object.ccpe_caloco[1]="M" OR dw_1.Object.ccpe_caloca[1]="M" Then
				dw_1.Object.ccpe_reslot[1]="O"
				dw_1.Object.ccpe_respri[1]="O"
				ii_objetados=1
	
				dw_1.Object.ccpe_reslot.Protect	=	1
				dw_1.Object.ccpe_causal.Protect	=	0
				dw_1.Object.ccpe_causal.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_causa2.Protect	=	0
				dw_1.Object.ccpe_causa2.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_causa3.Protect	=	0
				dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_respri.Protect	=	0
				dw_1.Object.ccpe_respri.BackGround.Color	=	RGB(255,255,255)
				
				dw_1.Object.ccpe_pocau1.Protect	=	0
				dw_1.Object.ccpe_pocau1.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_pocau2.Protect	=	0
				dw_1.Object.ccpe_pocau2.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_pocau3.Protect	=	0
				dw_1.Object.ccpe_pocau3.BackGround.Color	=	RGB(255,255,255)
	
			ELSE
				If iuo_Objetados.Existe(This.Object.clie_codigo[1], This.Object.plde_codigo[1], &
											This.Object.cclo_numero[1], This.Object.ccpe_numero[1], False, SQLCA) Then 
					MessageBox('Error', 'No se puede Modificar la planilla:' + String (This.Object.ccpe_numero[1], '00000000') + &
								',~r~npor que tiene Lotes Objetados Pendiente.~r~nResolucion:' + String (iuo_Objetados.ccte_numero, '00000000'), StopSign!, OK!)
					This.SetItem(Row, ls_Columna, 'M')
					Return 1
				End If
				
				dw_1.Object.ccpe_reslot[1]="A"
				dw_1.Object.ccpe_respri[1]= ls_Null
				dw_1.SetItem(1, "ccpe_causal",Integer(ls_null))
				dw_1.SetItem(1, "ccpe_pocau1",Integer(ls_null))
				dw_1.Object.ccpe_reslot.Protect	=	1
				dw_1.Object.ccpe_respri.Protect	=	1
				ii_objetados=0
			End If
		End If 			
		
	Case	"ccpe_caloca"	
		If data="M" Then
			dw_1.Object.ccpe_reslot[1]="O"
			dw_1.Object.ccpe_respri[1]="O"

			dw_1.Object.ccpe_causal.Protect	=	0
			dw_1.Object.ccpe_causal.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_causa2.Protect	=	0
			dw_1.Object.ccpe_causa2.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_causa3.Protect	=	0
			dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_respri.Protect	=	0
			dw_1.Object.ccpe_respri.BackGround.Color	=	RGB(255,255,255)
			
			dw_1.Object.ccpe_pocau1.Protect	=	0
			dw_1.Object.ccpe_pocau1.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_pocau2.Protect	=	0
			dw_1.Object.ccpe_pocau2.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_pocau3.Protect	=	0
			dw_1.Object.ccpe_pocau3.BackGround.Color	=	RGB(255,255,255)
			ii_objetados=1
			dw_1.Object.ccpe_reslot.Protect	=	1
			
		ELSE
			
			If dw_1.Object.ccpe_caloco[1]="M" OR dw_1.Object.ccpe_caloem[1]="M" Then
				dw_1.Object.ccpe_reslot[1]="O"
				dw_1.Object.ccpe_respri[1]="O"


				dw_1.Object.ccpe_causal.Protect	=	0
				dw_1.Object.ccpe_causal.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_causa2.Protect	=	0
				dw_1.Object.ccpe_causa2.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_causa3.Protect	=	0
				dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_respri.Protect	=	0
				dw_1.Object.ccpe_respri.BackGround.Color	=	RGB(255,255,255)
				
				dw_1.Object.ccpe_pocau1.Protect	=	0
				dw_1.Object.ccpe_pocau1.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_pocau2.Protect	=	0
				dw_1.Object.ccpe_pocau2.BackGround.Color	=	RGB(255,255,255)
				dw_1.Object.ccpe_pocau3.Protect	=	0
				dw_1.Object.ccpe_pocau3.BackGround.Color	=	RGB(255,255,255)
				ii_objetados=1
				dw_1.Object.ccpe_reslot.Protect	=	1
			ELSE
				If iuo_Objetados.Existe(This.Object.clie_codigo[1], This.Object.plde_codigo[1], &
											This.Object.cclo_numero[1], This.Object.ccpe_numero[1], False, SQLCA) Then 
					MessageBox('Error', 'No se puede eliminar la planilla:' + String (This.Object.ccpe_numero[1], '00000000') + &
								',~r~npor que tiene Lotes Objetados Pendiente.~r~nResolucion:' + String (iuo_Objetados.ccte_numero, '00000000'), StopSign!, OK!)
					This.SetItem(Row, ls_Columna, 'M')
					Return 1
				End If
				dw_1.Object.ccpe_reslot[1]="A"
				dw_1.Object.ccpe_respri[1]= ls_null
				dw_1.SetItem(il_fila, "ccpe_causa2",Integer(ls_null))
				dw_1.SetItem(il_fila, "ccpe_pocau2",Integer(ls_null))
				dw_1.Object.ccpe_reslot.Protect	=	1
				dw_1.Object.ccpe_respri.Protect	=	1
				ii_objetados=0
			End If
		End If 
		
	Case	"ccpe_caloco"	
		If data="M" Then
			dw_1.Object.ccpe_reslot[1]="O"
			dw_1.Object.ccpe_respri[1]="O"

			dw_1.Object.ccpe_causal.Protect	=	0
			dw_1.Object.ccpe_causal.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_causa2.Protect	=	0
			dw_1.Object.ccpe_causa2.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_causa3.Protect	=	0
			dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_respri.Protect	=	0
			dw_1.Object.ccpe_respri.BackGround.Color	=	RGB(255,255,255)
			
			dw_1.Object.ccpe_pocau1.Protect	=	0
			dw_1.Object.ccpe_pocau1.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_pocau2.Protect	=	0
			dw_1.Object.ccpe_pocau2.BackGround.Color	=	RGB(255,255,255)
			dw_1.Object.ccpe_pocau3.Protect	=	0
			dw_1.Object.ccpe_pocau3.BackGround.Color	=	RGB(255,255,255)
			ii_objetados=1 
			dw_1.Object.ccpe_reslot.Protect	=	1
		ELSE
				If dw_1.Object.ccpe_caloca[1]="M" OR dw_1.Object.ccpe_caloem[1]="M" Then
					dw_1.Object.ccpe_reslot[1]="O"
					dw_1.Object.ccpe_respri[1]="O"

					ii_objetados=1
					dw_1.Object.ccpe_causal.Protect	=	0
					dw_1.Object.ccpe_causal.BackGround.Color	=	RGB(255,255,255)
					dw_1.Object.ccpe_causa2.Protect	=	0
					dw_1.Object.ccpe_causa2.BackGround.Color	=	RGB(255,255,255)
					dw_1.Object.ccpe_causa3.Protect	=	0
					dw_1.Object.ccpe_causa3.BackGround.Color	=	RGB(255,255,255)
					dw_1.Object.ccpe_respri.Protect	=	0
					dw_1.Object.ccpe_respri.BackGround.Color	=	RGB(255,255,255)
					
					dw_1.Object.ccpe_pocau1.Protect	=	0
					dw_1.Object.ccpe_pocau1.BackGround.Color	=	RGB(255,255,255)
					dw_1.Object.ccpe_pocau2.Protect	=	0
					dw_1.Object.ccpe_pocau2.BackGround.Color	=	RGB(255,255,255)
					dw_1.Object.ccpe_pocau3.Protect	=	0
					dw_1.Object.ccpe_pocau3.BackGround.Color	=	RGB(255,255,255)
				
					dw_1.Object.ccpe_reslot.Protect	=	1
				ELSE
					If iuo_Objetados.Existe(This.Object.clie_codigo[1], This.Object.plde_codigo[1], &
												This.Object.cclo_numero[1], This.Object.ccpe_numero[1], False, SQLCA) Then 
						MessageBox('Error', 'No se puede eliminar la planilla:' + String (This.Object.ccpe_numero[1], '00000000') + &
									',~r~npor que tiene Lotes Objetados Pendiente.~r~nResolucion:' + String (iuo_Objetados.ccte_numero, '00000000'), StopSign!, OK!)
						This.SetItem(Row, ls_Columna, 'M')
						Return 1
					End If
				
					dw_1.Object.ccpe_reslot[1]="A"
					dw_1.Object.ccpe_respri[1]= ls_Null
					dw_1.SetItem(il_fila, "ccpe_causa3",Integer(ls_null))
					dw_1.SetItem(il_fila, "ccpe_pocau3",Integer(ls_null))
					ii_objetados=0
					dw_1.Object.ccpe_reslot.Protect	=	1
					dw_1.Object.ccpe_respri.Protect	=	1
				End If
		End If

	Case 'ccpe_catcon'
		If (Integer(Data) < 1 Or Integer(Data) > 4) Or Integer(Data) = Integer(ls_Null) Then 
			MessageBox('Atencion', 'Valor debe estar entre 1 y 4, o Nulo')
			dw_1.SetItem(il_fila, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
	Case 'ccpe_porapr'
		If Integer(data) > 100.00 Then
			MessageBox('Atencion', 'Valor no puede ser mayor a 100') 
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1 
		End If 				

End Choose

This.PostEvent('ue_valida')
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

