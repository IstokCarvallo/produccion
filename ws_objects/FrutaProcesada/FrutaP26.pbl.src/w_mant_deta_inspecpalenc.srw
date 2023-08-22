$PBExportHeader$w_mant_deta_inspecpalenc.srw
forward
global type w_mant_deta_inspecpalenc from w_mant_detalle
end type
end forward

global type w_mant_deta_inspecpalenc from w_mant_detalle
integer width = 2386
integer height = 1224
end type
global w_mant_deta_inspecpalenc w_mant_deta_inspecpalenc

type variables
DataWindowChild	dwc_especie, dwc_variedad, dwc_embalaje, dwc_tipopalemb, dwc_calibre
end variables

forward prototypes
public function boolean noexisteendddw (string as_columna, string as_valor, string as_nombre, boolean ab_string)
public function boolean duplicado (string as_columna, string as_valor)
public subroutine wf_opciontodas ()
public function boolean noexistecalibre (string as_calibre)
public function boolean noexisteembalaje (string as_embalaje)
end prototypes

public function boolean noexisteendddw (string as_columna, string as_valor, string as_nombre, boolean ab_string);DataWindowChild	ldwc_1

String	ls_busca
Long		ll_fila

dw_1.GetChild(as_columna,ldwc_1)

IF ab_string THEN
	ls_busca	=	as_columna + " = '" + as_valor + "'"
ELSE
	ls_busca	=	as_columna + " = " + as_valor
END IF

IF Isnull(ls_busca) THEN
	ll_fila = 1
ELSE
	ll_fila	=	ldwc_1.Find(ls_busca,1,ldwc_1.RowCount())
END IF

IF ll_fila = 0 THEN
	MessageBox("Atención ", as_nombre + " no ha sido ingresado.", Exclamation!, OK!) 
	RETURN True
ELSE
	IF as_nombre = 'Variedad' THEN
		dw_1.SetItem(il_fila,"vari_nombre",ldwc_1.GetItemString(ll_fila,"vari_nombre"))
	ELSEIF as_nombre = 'Tipo de Pallet' THEN
		dw_1.SetItem(il_fila,"tpem_cancaj",ldwc_1.GetItemNumber(ll_fila,"tpem_cancaj"))
	END IF
	RETURN False
END IF
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_fila
String	ls_especie, ls_variedad, ls_embalaje, ls_tipopalemb, ls_calibre

ls_especie		=	String(dw_1.Object.espe_codigo[il_fila])
ls_variedad		=	String(dw_1.Object.vari_codigo[il_fila])
ls_embalaje		=	dw_1.Object.emba_codigo[il_fila]
ls_tipopalemb	=	dw_1.Object.tpem_codigo[il_fila]
ls_calibre		=	dw_1.Object.inpe_calibr[il_fila]

CHOOSE CASE as_columna
	CASE "espe_codigo"
		ls_especie	=	as_valor
		
	CASE "vari_codigo"
		ls_variedad	=	as_valor
		
	CASE "emba_codigo"
		ls_embalaje		=	as_valor
		
	CASE "tpem_codigo"
		ls_tipopalemb	=	as_valor

	CASE "inpe_calibr"
		ls_calibre		=	as_valor		
		
END CHOOSE

IF IsNull(ls_Especie) OR ls_Especie = "" THEN
	ls_Especie	=	""
ELSE
	ls_Especie	=	"espe_codigo = " + ls_Especie + " and "
END IF

IF IsNull(ls_Variedad) OR ls_Variedad = "" THEN
	ls_Variedad	=	""
ELSE
	ls_Variedad	=	"vari_codigo = " + ls_Variedad + " and "
END IF

IF IsNull(ls_calibre) OR ls_calibre = "" THEN
	ls_calibre	=	""
ELSE
	ls_calibre	=	"inpe_calibr = '" + ls_calibre + "' and "
END IF

ll_fila	= istr_mant.dw.Find(ls_especie + ls_variedad + ls_calibre + &
							"emba_codigo = '" + ls_embalaje + "' and " + &
							"tpem_codigo = '" + ls_tipopalemb + "'", &	
							1, istr_mant.dw.RowCount())

IF ll_fila > 0 and ll_fila <> il_filaAnc THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine wf_opciontodas ();Long		ll_fila
Integer	li_null
String	ls_null

SetNull(li_null)
SetNull(ls_null)

//Inserción de Opción Todas

//ll_fila	= dwc_especie.InsertRow(1)
//dwc_especie.SetItem(ll_fila,"espe_codigo",li_null)
//dwc_especie.SetItem(ll_fila,"espe_nombre","TODAS")
//
//ll_fila	= dwc_variedad.InsertRow(1)
//dwc_variedad.SetItem(ll_fila,"vari_codigo",li_null)
//dwc_variedad.SetItem(ll_fila,"vari_nombre","TODAS")
end subroutine

public function boolean noexistecalibre (string as_calibre);String		ls_nombre
Integer		li_Cliente, li_Especie, li_Variedad

li_Cliente	=	Integer(istr_mant.Argumento[1])
li_Especie	=	dw_1.Object.espe_codigo[il_Fila]
li_Variedad	=	dw_1.Object.vari_codigo[il_Fila]

IF IsNull(li_Especie) = False OR li_Especie > 0 OR &
	IsNull(li_Variedad) = False OR li_Variedad > 0 OR &
	IsNull(as_Calibre) = False OR as_Calibre = '' THEN
	SELECT	vaca_calibr
		INTO	:ls_nombre
		FROM	dbo.variecalibre
		WHERE	espe_codigo	=	:li_Especie
		AND	vari_codigo =	:li_Variedad
		AND	vaca_calibr =	:as_Calibre;

	IF sqlca.SQLCode = -1 THEN
  		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla variecalibre")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Calibre no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
		
		RETURN True
	ELSE
		
		RETURN False
	END IF
ELSE
	RETURN True
END IF
end function

public function boolean noexisteembalaje (string as_embalaje);Integer li_fila, li_cliente

li_cliente = Integer(istr_mant.Argumento[1])

IF IsNull(as_embalaje) or Trim(as_embalaje) = "" THEN
	RETURN TRUE
ELSE
	
	SELECT	Count(*)
		INTO	:li_fila
		FROM	dbo.embalajesprod
		WHERE	emba_codigo	=	:as_embalaje
		AND	clie_codigo =	:li_cliente;
		
	IF sqlca.SQLCode = -1 THEN
  		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla EmbalajesProd")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Embalaje no ha sido Definido para el cliente.~r~r" + &
					"Ingrese o seleccione otro Código.")
		
		RETURN True
	ELSE
		
		RETURN False
	END IF
END IF

end function

event open;Long	li_cliente

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

li_cliente	=	Integer(istr_mant.argumento[1])

dw_1.GetChild("espe_codigo",dwc_especie)
dw_1.GetChild("vari_codigo",dwc_variedad)
dw_1.GetChild("emba_codigo",dwc_embalaje)
dw_1.GetChild("tpem_codigo",dwc_tipopalemb)
dw_1.GetChild("inpe_calibr",dwc_calibre)

dwc_especie.SetTransObject(Sqlca)
dwc_variedad.SetTransObject(Sqlca)
dwc_embalaje.SetTransObject(Sqlca)
dwc_tipopalemb.SetTransObject(Sqlca)
dwc_calibre.SetTransObject(Sqlca)

dwc_especie.Retrieve()				//Especies del Cliente
dwc_variedad.Retrieve(gi_Codespecie)			//Variedades del Cliente y la Especie
dwc_embalaje.Retrieve(li_cliente)			//Embalajes del Cliente
dwc_tipopalemb.Retrieve(li_cliente,'Z')	//Tipos de Pallet por Embalajes del Cliente
dwc_calibre.Retrieve(gi_CodEspecie,gi_CodVariedad) // Calibre

wf_opciontodas()

dw_1.SetTransObject(sqlca)
end event

on w_mant_deta_inspecpalenc.create
call super::create
end on

on w_mant_deta_inspecpalenc.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[2]	=	String(dw_1.Object.espe_codigo[il_fila])
ias_campo[3]	=	String(dw_1.Object.vari_codigo[il_fila])
ias_campo[4]	=	dw_1.Object.emba_codigo[il_fila]
ias_campo[5]	=	dw_1.Object.tpem_codigo[il_fila]
ias_campo[6]	=	dw_1.Object.inpe_calibr[il_fila]
ias_campo[7]	=	dw_1.Object.inpe_solnom[il_fila]

IF istr_mant.agrega THEN
	dw_1.Object.inpe_tipoin[il_fila]	=	Integer(istr_mant.argumento[3])
	dw_1.Object.inpe_numero[il_fila]	=	Long(istr_mant.argumento[4])
	dw_1.Object.clie_codigo[il_fila]	=	Integer(istr_mant.argumento[1])
	dw_1.Object.plde_codigo[il_fila]	=	Integer(istr_mant.argumento[2])
	dw_1.Object.dest_codigo[il_fila]	=	Integer(istr_mant.argumento[5])
	dw_1.Object.inpe_fechai[il_fila]	=	Date(istr_mant.argumento[6])
	dw_1.Object.inpe_solnom[il_fila]	=	istr_mant.argumento[7]
	//dw_1.Object.variedades_vari_nombre[il_fila]	=	'TODAS'

	dw_1.SetItemStatus(il_fila,"inpe_tipoin",Primary!,NotModified!)
	dw_1.SetItemStatus(il_fila,"inpe_numero",Primary!,NotModified!)
	dw_1.SetItemStatus(il_fila,"clie_codigo",Primary!,NotModified!)
	dw_1.SetItemStatus(il_fila,"plde_codigo",Primary!,NotModified!)
	dw_1.SetItemStatus(il_fila,"dest_codigo",Primary!,NotModified!)
	dw_1.SetItemStatus(il_fila,"inpe_fechai",Primary!,NotModified!)
	dw_1.SetItemStatus(il_fila,"inpe_solnom",Primary!,NotModified!)
	//dw_1.SetItemStatus(il_fila,"variedades_vari_nombre",Primary!,NotModified!)
END IF	
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.inpe_tipoin[il_fila]	=	Integer(istr_mant.argumento[3])
dw_1.Object.inpe_numero[il_fila]	=	Long(istr_mant.argumento[4])
dw_1.Object.clie_codigo[il_fila]	=	Integer(istr_mant.argumento[1])
dw_1.Object.plde_codigo[il_fila]	=	Integer(istr_mant.argumento[2])
dw_1.Object.dest_codigo[il_fila]	=	Integer(istr_mant.argumento[5])
dw_1.Object.inpe_fechai[il_fila]	=	Date(istr_mant.argumento[6])
dw_1.Object.inpe_solnom[il_fila]	=	istr_mant.argumento[7]
//dw_1.Object.variedades_vari_nombre[il_fila]	=	'TODAS'

dw_1.SetItemStatus(il_fila,"inpe_tipoin",Primary!,NotModified!)
dw_1.SetItemStatus(il_fila,"inpe_numero",Primary!,NotModified!)
dw_1.SetItemStatus(il_fila,"clie_codigo",Primary!,NotModified!)
dw_1.SetItemStatus(il_fila,"plde_codigo",Primary!,NotModified!)
dw_1.SetItemStatus(il_fila,"dest_codigo",Primary!,NotModified!)
dw_1.SetItemStatus(il_fila,"inpe_fechai",Primary!,NotModified!)
dw_1.SetItemStatus(il_fila,"inpe_solnom",Primary!,NotModified!)
//dw_1.SetItemStatus(il_fila,"variedades_vari_nombre",Primary!,NotModified!)

dw_1.SetColumn("espe_codigo")
end event

event ue_deshace;call super::ue_deshace;dw_1.Object.espe_codigo[il_fila]	=	Integer(ias_campo[2])
dw_1.Object.vari_codigo[il_fila]	=	Integer(ias_campo[3])
dw_1.Object.emba_codigo[il_fila]	=	ias_campo[4]
dw_1.Object.tpem_codigo[il_fila]	=	ias_campo[5]
dw_1.Object.inpe_calibr[il_fila]	=	ias_campo[6]
dw_1.Object.inpe_solnom[il_fila]	=	ias_campo[7]
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_inspecpalenc
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_inspecpalenc
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_inspecpalenc
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_inspecpalenc
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_inspecpalenc
integer x = 1979
integer y = 404
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_inspecpalenc
integer x = 1979
integer y = 156
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_inspecpalenc
integer x = 1979
integer y = 660
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_inspecpalenc
integer width = 1787
integer height = 948
string dataobject = "dw_mant_inspecpalenc"
end type

event dw_1::itemchanged;call super::itemchanged;Integer	li_null, li_especie, li_variedad
Long		ll_fila
String	ls_columna,ls_null

SetNull(li_null)
SetNull(ls_null)

ls_columna	=	dwo.Name

CHOOSE CASE ls_columna
	CASE "espe_codigo"
		IF Noexisteendddw(ls_columna,data,"Especie",False) OR Duplicado(ls_columna,data) THEN
			This.SetItem(row,ls_columna,Integer(ias_campo[2]))
			RETURN 1
//		ELSEIF Isnull(data) THEN
//			This.SetTabOrder("vari_codigo",0)
//			This.SetItem(row,"vari_codigo",li_null)
//			This.SetItem(row,"variedades_vari_nombre",'TODAS')
		ELSE
			li_especie = Integer(data)
			dwc_variedad.Retrieve(li_especie)
//			ll_fila	= dwc_variedad.InsertRow(1)
//			dwc_variedad.SetItem(ll_fila,"vari_codigo",li_null)
//			dwc_variedad.SetItem(ll_fila,"vari_nombre","TODAS")
//			This.SetTabOrder("vari_codigo",20)
		END IF
	CASE "vari_codigo"
		
		IF Noexisteendddw(ls_columna,data,"Variedad",False) OR Duplicado(ls_columna,data) THEN
			This.SetItem(row,ls_columna,Integer(ias_campo[3]))
			RETURN 1
		ELSE
			li_especie = Integer(This.Object.espe_codigo[row])
			li_variedad= Integer(data)
			dwc_calibre.Retrieve(li_especie,li_variedad)
		END IF
		
	CASE "inpe_calibr"
		IF NoExisteCalibre(data) OR Duplicado(ls_Columna, data) THEN
			This.SetItem(il_fila, "inpe_calibr", ls_Null)
			RETURN 1
		END IF	

   CASE "emba_codigo"
		IF noexisteembalaje(data) OR Duplicado(ls_columna,data) THEN
			This.SetItem(row,ls_columna,ias_campo[4])
			RETURN 1
		ELSE
			dwc_tipopalemb.Reset()
			dwc_tipopalemb.Retrieve(Integer(istr_mant.argumento[1]),data)
			This.SetItem(row,"tpem_codigo",ls_null)
		END IF
	CASE "tpem_codigo"
		IF Noexisteendddw(ls_columna,data,"Tipo de Pallet",True) OR Duplicado(ls_columna,data) THEN
         pb_salir.TriggerEvent(Clicked!)	
			//This.SetItem(row,ls_columna,ls_null)
			//RETURN 1
		END IF
		
END CHOOSE
end event

