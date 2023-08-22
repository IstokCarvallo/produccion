$PBExportHeader$w_mant_deta_planificallegadadeta.srw
forward
global type w_mant_deta_planificallegadadeta from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_planificallegadadeta from w_mant_detalle_csd
integer width = 2789
integer height = 1396
end type
global w_mant_deta_planificallegadadeta w_mant_deta_planificallegadadeta

type variables
Integer ii_estado
end variables

forward prototypes
public subroutine buscavariedad ()
public function boolean existevariedad (string ls_columna)
public subroutine buscaembalaje ()
public function boolean existeembalaje (string ls_columna)
public function boolean existeespecie (integer ai_codigo)
public function boolean duplicado (string as_codigo)
end prototypes

public subroutine buscavariedad ();Str_busqueda	lstr_busq

dw_1.Modify("buscavariedad.border = 0")
dw_1.Modify("buscavariedad.border = 5")

lstr_busq.argum[2]	=	String(dw_1.Object.espe_codigo[il_fila])
lstr_busq.argum[1]	=	String(dw_1.Object.espe_codigo[il_fila])

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] <> '' THEN
	istr_mant.argumento[5]	=	lstr_busq.argum[4]
	istr_mant.argumento[6]	=	lstr_busq.argum[5]
	
	dw_1.setItem(il_fila, "vari_codigo", Integer(lstr_busq.argum[4]))
	dw_1.setItem(il_fila, "vari_nombre", lstr_busq.argum[5])
	dw_1.SetColumn("vari_codigo")
	dw_1.SetFocus()
END IF

dw_1.Modify("buscavariedad.border = 0")
dw_1.Modify("buscavariedad.border = 6")
end subroutine

public function boolean existevariedad (string ls_columna);Integer	li_cliente, li_especie, li_variedad
String	ls_nombre

li_cliente		=	Integer(istr_mant.argumento[1])
li_especie		=	dw_1.Object.espe_codigo[il_fila]
li_variedad		=	Integer(ls_columna)

SELECT	vari_nombre
	INTO	:ls_nombre
	FROM	dbo.variedades
	WHERE	espe_codigo	= 	:li_especie
	AND	vari_codigo = 	:li_variedad ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variedades")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Variedad no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN False
ELSE
	dw_1.SetItem(il_fila, "vari_nombre", ls_nombre)
	//dw_seca.Retrieve(gi_codexport,li_especie,li_variedad)
	RETURN True
END IF

RETURN False
end function

public subroutine buscaembalaje ();Str_busqueda		lstr_busq

dw_1.Modify("buscaembalaje.border = 0")
dw_1.Modify("buscaembalaje.border = 5")

lstr_busq.Argum[1]	=	istr_mant.Argumento[1]

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.Argum[2] <> "" THEN
	istr_mant.Argumento[7]	=	lstr_busq.Argum[2]
	istr_mant.Argumento[8]	=	lstr_busq.Argum[3]
	
	dw_1.setItem(il_fila, "emba_codigo", lstr_busq.Argum[2])
			
ELSE
	dw_1.SetColumn("emba_codigo")
	dw_1.SetFocus()
END IF

dw_1.Modify("buscaembalaje.border = 0")
dw_1.Modify("buscaembalaje.border = 6")
end subroutine

public function boolean existeembalaje (string ls_columna);String	ls_codigo, ls_nombre
Integer	li_cliente, li_tipoen

li_cliente	= Integer(istr_mant.argumento[1])
ls_codigo	= ls_columna

SELECT	emb.emba_nombre, env.enva_tipoen
	INTO 	:ls_nombre, :li_tipoen
	FROM	dbo.embalajesprod as emb, dbo.envases as env
	WHERE	emb.clie_codigo	= :li_cliente
	AND	emb.emba_codigo	= :ls_codigo
	AND	env.enva_tipoen	= emb.enva_tipoen
	AND	env.enva_codigo	= emb.enva_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Emabalajes")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embalaje no Existe para este Cliente. Ingrese Otro.", Exclamation!, Ok!)
	RETURN False
ELSE
	
	RETURN True
END IF

end function

public function boolean existeespecie (integer ai_codigo);Integer	li_cont

SELECT	count(*)
	INTO	:li_cont
	FROM	dbo.especies
	WHERE	espe_codigo	= 	:ai_codigo;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla especies")
	RETURN True
ELSEIF li_cont = 0 THEN
	MessageBox("Atención","Código de Especie no Existe. Ingrese Otro.", Exclamation!, Ok!)
	RETURN True
END IF

RETURN False
end function

public function boolean duplicado (string as_codigo);Long		ll_fila
Integer	li_especie, li_variedad, li_cantid
String	ls_embalaje

li_especie 	= 	dw_1.Object.espe_codigo[il_fila]
li_variedad	= 	dw_1.Object.vari_codigo[il_fila]
ls_embalaje	=	dw_1.Object.emba_codigo[il_fila]

IF isnull(dw_1.Object.espe_codigo[il_fila]) THEN
	li_especie = Integer(as_codigo)
END IF
IF isnull(dw_1.Object.vari_codigo[il_fila]) THEN
	li_variedad = Integer(as_codigo)
END IF
IF isnull(dw_1.Object.emba_codigo[il_fila]) THEN
	ls_embalaje = as_codigo
END IF

ll_fila	=	dw_1.Find("espe_codigo = " + String(li_especie) + &
						" AND vari_codigo = " + String(li_variedad) + &
						" AND emba_codigo = '" + ls_embalaje + "'" , 1, dw_1.RowCount())
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue incluido", Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

on w_mant_deta_planificallegadadeta.create
call super::create
end on

on w_mant_deta_planificallegadadeta.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]	=	String(dw_1.Object.clie_codigo[il_fila])
ias_campo[2]	=	String(dw_1.Object.plde_codigo[il_fila])
ias_campo[3]	=	String(dw_1.Object.plan_copack[il_fila])
ias_campo[4]	=	String(dw_1.Object.plan_fechal[il_fila])
ias_campo[5]	=	String(dw_1.Object.plan_horall[il_fila])

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "plan_copack", Long(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "plan_fechal", date(istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "plan_horall", Time(istr_mant.argumento[5]))
END IF

end event

event ue_deshace;call super::ue_deshace;dw_1.Object.clie_codigo[il_fila]	=	Integer(ias_campo[1])
dw_1.Object.plde_codigo[il_fila]	=	Integer(ias_campo[2])
dw_1.Object.plan_copack[il_fila]	=	Integer(ias_campo[3])
dw_1.Object.plan_fechal[il_fila]	=	Date(ias_campo[4])
dw_1.Object.plan_horall[il_fila]	=	Time(ias_campo[5])


end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.espe_codigo[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Especie"
	ls_colu[li_cont]	= "espe_codigo"
END IF

IF Isnull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Variedad"
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF Isnull(dw_1.Object.emba_codigo[il_fila]) OR dw_1.Object.emba_codigo[il_fila] = '' THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Embalaje"
	ls_colu[li_cont]	= "emba_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus() 
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;ib_ok = True
Long		ll_cajas

dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "clie_codigo", integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "plan_copack", Integer(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "plan_fechal", date(istr_mant.argumento[4]))
dw_1.SetItem(il_fila, "plan_horall", Time(istr_mant.argumento[5]))

dw_1.SetColumn("espe_codigo")
dw_1.SetFocus()

end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_planificallegadadeta
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_planificallegadadeta
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_planificallegadadeta
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_planificallegadadeta
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_planificallegadadeta
integer x = 2441
integer y = 368
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_planificallegadadeta
integer x = 2441
integer y = 152
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_planificallegadadeta
integer x = 2441
integer y = 584
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_planificallegadadeta
integer x = 91
integer y = 96
integer width = 2190
integer height = 1132
string dataobject = "dw_mant_planificallegadadeta"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_Nula
Integer	li_cliente,pallet,li_status,li_tipo
Long		ll_null

SetNull(ls_Nula)
SetNull(ll_null)

ls_columna = dwo.name

CHOOSE CASE ls_columna
			
	CASE "espe_codigo"
		IF existeespecie(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, Integer(ll_null))
			RETURN 1
		END IF	
		
		IF duplicado(data) THEN
			This.SetItem(Row, ls_Columna, Integer(ll_null))
			RETURN 1
		END IF
		
		istr_mant.argumento[5]	= data
		dw_1.SetItem(row, "vari_nombre", "")
		dw_1.SetItem(row, "vari_codigo", ll_null)
				
	CASE "vari_codigo"
		IF ExisteVariedad(data) = False THEN
			dw_1.SetItem(row, "vari_nombre", "")
			dw_1.SetItem(row, "vari_codigo", ll_null)
			RETURN 1
		END IF
		
		IF duplicado(data) THEN
			This.SetItem(Row, ls_Columna, Integer(ll_null))
			RETURN 1
		END IF
	
	CASE "emba_codigo"	
		IF ExisteEmbalaje(data) = False THEN
			dw_1.SetItem(row, "emba_codigo", "")
			RETURN 1
		END IF
		
		IF duplicado(data) THEN
			This.SetItem(Row, ls_Columna, String(ll_null))
			RETURN 1
		END IF
		
END CHOOSE
end event

event dw_1::clicked;call super::clicked;CHOOSE CASE dwo.name
		
	CASE "buscavariedad"
		buscavariedad()
		
	CASE "buscaembalaje"
		buscaembalaje()
		
END CHOOSE
end event

