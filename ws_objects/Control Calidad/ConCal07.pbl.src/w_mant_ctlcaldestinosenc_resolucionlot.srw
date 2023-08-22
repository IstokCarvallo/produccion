$PBExportHeader$w_mant_ctlcaldestinosenc_resolucionlot.srw
forward
global type w_mant_ctlcaldestinosenc_resolucionlot from w_mant_detalle_csd
end type
end forward

global type w_mant_ctlcaldestinosenc_resolucionlot from w_mant_detalle_csd
integer x = 123
integer y = 96
integer width = 2446
integer height = 1004
string title = "Resolución"
boolean controlmenu = true
end type
global w_mant_ctlcaldestinosenc_resolucionlot w_mant_ctlcaldestinosenc_resolucionlot

type variables
uo_ctlcaldanoespecie		iuo_ctlcaldanoespecie
end variables

forward prototypes
public function boolean noexistecliente (integer ai_cliente)
public subroutine wf_nuevo ()
public function boolean duplicado (string as_valor, integer ai_tipo)
public function boolean Causales (integer ai_causa)
protected function boolean noexisteproductor (long al_productor)
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

public function boolean Causales (integer ai_causa);Integer	li_Causal, li_Existe 

li_Causal	=	ai_causa

SELECT Count(*)
INTO	 :li_Existe 
FROM	 dba.ctlcaldanoespecie
WHERE	 espe_codigo	=	11
AND    clie_codigo	=	81
AND    ccda_secuen	=	:li_Causal;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la Tabla de Daños por Especie")
ELSEIF li_Existe	>	0	THEN
	RETURN TRUE
ELSE
	MessageBox("Atención","Causal digitado, no se encuentra Ingresado en tabla respectiva",StopSign!, Ok!)
END IF
RETURN FALSE


end function

protected function boolean noexisteproductor (long al_productor);Integer  li_Cliente
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

on w_mant_ctlcaldestinosenc_resolucionlot.create
call super::create
end on

on w_mant_ctlcaldestinosenc_resolucionlot.destroy
call super::destroy
end on

event ue_recuperadatos();String ls_Null

SetNull(ls_Null)

w_main.SetMicroHelp("Recuperando Datos...")

SetPointer(HourGlass!)
PostEvent("ue_listo")

IF istr_mant.Agrega	THEN
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
	ELSE
		pb_Salir.Enabled	=	False
	END IF
END IF

ias_Campo[1]	=	dw_1.Object.ccde_reslot[il_fila]
ias_Campo[2]	= String(dw_1.Object.ccde_cauobj[il_fila])

IF NOT IsNull(dw_1.Object.ccde_reslot[il_fila])	THEN 
	IF dw_1.Object.ccde_reslot[il_fila]	=	"0"	THEN 		
		dw_1.Object.ccde_cauobj.Protect	=	0
		dw_1.object.ccde_cauobj.BackGround.Color	=	RGB(255,255,255)		
		dw_1.Object.ccde_causa2.Protect	=	0	
		dw_1.Object.ccde_causa2.BackGround.Color	=	RGB(255,255,255)		
		dw_1.Object.ccde_causa3.Protect	=	0
		dw_1.	Object.ccde_causa3.BackGround.Color	=	RGB(255,255,255)		
	ELSE	
		dw_1.Object.ccde_cauobj.Protect	=	1
		dw_1.object.ccde_cauobj.BackGround.Color	=	RGB(192,192,192)					
		dw_1.Object.ccde_causa2.Protect	=	1	
		dw_1.Object.ccde_causa3.BackGround.Color	=	RGB(192,192,192)		
		dw_1.Object.ccde_causa3.protect 	=	1
		dw_1.Object.ccde_causa3.BackGround.Color	=	RGB(192,192,192)
	END IF
END IF 	
end event

event ue_deshace();call super::ue_deshace;String	ls_NulA

SetNull(ls_Nula)

IF Upperbound(ias_campo) > 0 THEN 
	dw_1.SetItem(1, "ccde_reslot", ls_Nula)
	dw_1.Setitem(1, "ccde_cauobj", ls_Nula)
END IF	
end event

event ue_antesguardar();String	ls_Mensaje, ls_Columna[]
Integer	li_Contador

IF Isnull(dw_1.Object.ccde_reslot[1]) OR dw_1.Object.ccde_reslot[1] = "" THEN 
	li_contador ++
	ls_Mensaje	= ls_Mensaje	+ "~Resolución Lote"
	ls_Columna[li_Contador]	=	"ccde_reslot"
END IF	

IF Isnull(dw_1.Object.ccde_reslot[1])	= False AND dw_1.Object.ccde_reslot[1] = "O" THEN
	IF Isnull(dw_1.Object.ccde_cauobj[1]) OR dw_1.Object.ccde_cauobj[1]	=	0 	THEN 
		li_Contador ++
		ls_Mensaje 	=	ls_Mensaje  + "~Causal de Objeción 1"
		ls_ColumNa[li_contador]	=	"ccde_cauobj"
	END IF 	
END IF 	

IF li_contador > 0 THEN 
	Messagebox("Error deConsistencia", "Falta el ingreso de :" + ls_Mensaje + ".",StopSign!, Ok!)
	dw_1.setColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm	=	-1	
END IF 
end event

event open;x	= 100
y	= 450

/*
Argumento
23: Resolución Lote
*/

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")
istr_Mant = Message.PowerObjectParm
istr_Mant.dw2.ShareData(dw_1)

dw_1.SetTransObject(SqlCa)

iuo_ctlcaldanoespecie	=	Create uo_ctlcaldanoespecie

dw_1.Object.ccde_cauobj.Protect	=	1	
dw_1.Object.ccde_cauobj.BackGround.Color	=	RGB(192,192,192)

dw_1.Object.ccde_causa2.Protect	=	1
dw_1.Object.ccde_causa2.backGround.Color	=	RGB(192,192,192)

dw_1.Object.ccde_causa3.Protect	=	1	
dw_1.Object.ccde_causa3.BackGround.Color	=	RGB(192,192,192)

istr_mant.Argumento[23]	=	""
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_ctlcaldestinosenc_resolucionlot
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_ctlcaldestinosenc_resolucionlot
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_ctlcaldestinosenc_resolucionlot
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_ctlcaldestinosenc_resolucionlot
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_ctlcaldestinosenc_resolucionlot
integer x = 2162
integer y = 404
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_ctlcaldestinosenc_resolucionlot
integer x = 2162
integer y = 228
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_ctlcaldestinosenc_resolucionlot
integer x = 2162
integer y = 580
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_ctlcaldestinosenc_resolucionlot
integer x = 146
integer y = 104
integer width = 1902
integer height = 736
string dataobject = "dw_mant_ctlcaldestinosenc_resolucionlot"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Null

SetNull(ls_Null)

ls_Columna = dwo.Name

dw_1.AcceptText()

CHOOSE CASE ls_Columna
	
	CASE "ccde_reslot"		
		istr_mant.Argumento[19]	=	Data		
		IF Data	=	"O" THEN
			dw_1.Object.ccde_cauobj.Protect	=	0	
			dw_1.Object.ccde_cauobj.BackGround.Color	= 	RGB(255,255,255)				
			dw_1.Object.ccde_causa2.Protect	=	0			
			dw_1.Object.ccde_causa2.BackGround.Color	=	RGB(255,255,255)					
			dw_1.Object.ccde_causa3.Protect	=	0
			dw_1.Object.ccde_causa3.BackGround.Color	=	RGB(255,255,255)					
		ELSE			
			dw_1.Object.ccde_cauobj[il_fila]	=	Integer(ls_Null)
			dw_1.Object.ccde_causa2[il_fila]	=	Integer(ls_Null)
			dw_1.Object.ccde_causa3[il_fila]	=	Integer(ls_Null)						
			dw_1.Object.ccde_cauobj.Protect	=	1	
			dw_1.object.ccde_cauobj.backGround.Color	=	RGB(192,192,192)				
			dw_1.Object.ccde_causa2.Protect	=	1	
			dw_1.Object.ccde_causa2.BackGround.Color	=	RGB(192,192,192)					
			dw_1.Object.ccde_causa3.Protect	=	1	
			dw_1.Object.ccde_causa3.BackGround.color	=	RGB(192,192,192)							
		END IF

	CASE "ccde_cauobj","ccde_causa2","ccde_causa3"		
		IF NOT Causales(Integer(data)) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			RETURN 1 
		END IF 				
	
END CHOOSE
end event

