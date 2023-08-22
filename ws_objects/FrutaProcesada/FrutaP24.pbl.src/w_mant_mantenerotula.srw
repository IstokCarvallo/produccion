$PBExportHeader$w_mant_mantenerotula.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_mantenerotula from w_mant_directo
end type
type st_2 from statictext within w_mant_mantenerotula
end type
type st_1 from statictext within w_mant_mantenerotula
end type
type em_nroguia from editmask within w_mant_mantenerotula
end type
type st_3 from statictext within w_mant_mantenerotula
end type
type dw_3 from datawindow within w_mant_mantenerotula
end type
type dw_4 from datawindow within w_mant_mantenerotula
end type
end forward

global type w_mant_mantenerotula from w_mant_directo
integer x = 155
integer y = 156
integer width = 4965
integer height = 3048
string title = "MANTENEDOR DE ROTULADOS"
event ue_validaborrar ( )
st_2 st_2
st_1 st_1
em_nroguia em_nroguia
st_3 st_3
dw_3 dw_3
dw_4 dw_4
end type
global w_mant_mantenerotula w_mant_mantenerotula

type variables
DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta
Integer ii_pallet, ii_nuevo

//str_busqueda istr_busq
//str_mant istr_mant
Integer	ii_tipo, ii_contador
String	is_report



end variables

forward prototypes
public function boolean existeguia (long al_guia)
public function boolean noexistevariedad (string as_columna, string as_valor)
public function boolean existeproductor (string as_valor)
public function boolean existecalibre (string as_valor)
public function string f_variedadnom (integer codigo)
public function string f_prodnombre (integer li_codigo)
public subroutine buscavariedad ()
public subroutine buscaproductor ()
public function boolean existeembalaje (string as_embalaje, integer al_cliente)
end prototypes

public function boolean existeguia (long al_guia);Integer	li_codexp, li_planta, li_Tipova, li_tipoembq
Date		ld_fecha
String	ls_Embarque

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF al_guia <> 0 OR li_planta = 0 THEN



	SELECT defe_tiposa
		INTO	:li_tipoembq
		FROM	dba.DESPAFRIGOEN 
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND	defe_guides	=	:al_guia;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_nroguia.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Número Guia Despacho Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
//		pb_acepta.Enabled	= False
		em_nroguia.SetFocus()
		RETURN False
	    ELSEIF li_tipoembq = 7 OR li_tipoembq = 8 OR li_tipoembq = 9 THEN		
		
					SELECT embq_codigo
					INTO	:ls_Embarque
					FROM	dba.DESPAFRIGOEN 
					WHERE	plde_codigo =	:li_planta
					AND	clie_codigo	=	:li_codexp
					AND	defe_guides	=	:al_guia;
					
					
					SELECT embq_tipova
					INTO	:li_Tipova
					FROM	dba.EMBARQUEPROD 
					WHERE	clie_codigo	=	:li_codexp
					AND	embq_codigo	=	:ls_Embarque;
			
					IF sqlca.SQLCode = -1 THEN
						F_ErrorBaseDatos(sqlca,"Lectura de la Tabla EMBARQUEPROD")
						em_nroguia.SetFocus()
						RETURN False
					ELSEIF sqlca.SQLCode = 100 THEN
						MessageBox("Atención", "No existe Embarque.~r~r", &
										Exclamation!, Ok!)
//						pb_acepta.Enabled	= False
						em_nroguia.SetFocus()
						RETURN False
					ELSE
						istr_mant.argumento[25]	=	String(li_Tipova)
					
//						pb_acepta.Enabled	= True
						RETURN True
					END IF
				ELSE
			      istr_mant.argumento[25]	=	'1'
		      	RETURN True
		      END IF

ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

public function boolean noexistevariedad (string as_columna, string as_valor);Integer	li_Especie, li_Variedad, li_Existes
Boolean	lb_Retorno


li_Variedad	=	dw_1.Object.pafr_varrot[il_Fila]


CHOOSE CASE as_Columna
	CASE "pafr_varrot"
		li_Variedad	=	Integer(as_Valor)
		
		SELECT Count(*)
		INTO	 :li_Existes
		FROM	 dba.variedades
		WHERE  vari_codigo	=	:li_Variedad;
		
		IF sqlca.sqlcode	=	-1	THEN
			F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Variedades")
		ELSEIF 	li_Existes	=	0	THEN
			MessageBox("Atención","Código de Variedad no Existe.~r " +&
			"Ingrese Otro.")			
			lb_Retorno	=	TRUE
		END IF
		
END CHOOSE

RETURN lb_Retorno
end function

public function boolean existeproductor (string as_valor);Integer	li_productor, li_Existe, li_cliente
Boolean	lb_Retorno

li_cliente = dw_3.Object.clie_codigo[1]
li_productor	=	Integer(as_Valor)

SELECT  Count(*)
INTO	  :li_Existe
FROM    dba.productores as pro,dba.productoresclientes as cli
WHERE   pro.prod_codigo = :li_productor
AND	  pro.prod_codigo = cli.prod_codigo
AND	  cli.clie_codigo = :li_cliente;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Productores")
	lb_Retorno	=	True
ELSEIF li_Existe = 0 THEN
	lb_Retorno	=	True
	MessageBox("Atención","Código de productor no existe o pertenece a otro cliente")
END IF

RETURN lb_Retorno





end function

public function boolean existecalibre (string as_valor);Integer li_Exis, li_especie, li_cliente,li_varirotula
Boolean	lb_Retorno
string ls_calibre

li_cliente = dw_1.Object.clie_codigo[il_fila]
li_especie = dw_1.Object.espe_codigo[il_fila]
li_varirotula = dw_1.Object.pafr_varrot[il_fila]

ls_calibre	=	as_Valor

SELECT  Count(*)
INTO	  :li_Exis
FROM    dba.variecalibre
WHERE   vaca_calibr=:ls_calibre and
		  espe_codigo =:li_especie and
		  vari_codigo =:li_varirotula;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Variecalibre")
	lb_Retorno	=	True
ELSEIF li_Exis = 0 THEN
	lb_Retorno	=	True
	MessageBox("Atención","Calibre no Existe")
END IF

RETURN lb_Retorno

end function

public function string f_variedadnom (integer codigo);string ls_nombre
integer li_cliente, li_especie

li_cliente = dw_1.Object.clie_codigo[il_fila]
li_especie = dw_1.Object.espe_codigo[il_fila]

SELECT	vari_nombre
	INTO	:ls_nombre
	FROM	dba.variedades
	WHERE	vari_codigo	=	:codigo and
		   espe_codigo =	:li_especie;
		

	RETURN ls_nombre

end function

public function string f_prodnombre (integer li_codigo);string ls_nombre
integer li_cliente, li_especie

li_cliente = dw_1.Object.clie_codigo[il_fila]


SELECT	prod_nombre
	INTO	:ls_nombre
	FROM	dba.productores
	WHERE	prod_codigo	=	:li_codigo;
		
RETURN ls_nombre



end function

public subroutine buscavariedad ();string 	ls_null

SetNull(ls_null)

Str_busqueda	lstr_busq

dw_1.Modify("buscavariedad.border = 0")
dw_1.Modify("buscavariedad.border = 5")

lstr_busq.argum[1]	=	istr_mant.Argumento[1]
lstr_busq.argum[2]	=	String(dw_1.GetItemNumber(1, "espe_codigo"))

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] <> '' THEN
	istr_mant.argumento[4]	=	lstr_busq.argum[4]
	istr_mant.argumento[5]	=	lstr_busq.argum[5]
	
	dw_1.setItem(il_fila, "pafr_varrot", Integer(lstr_busq.argum[4]))
	dw_1.setItem(il_fila, "vari_nombre_1", lstr_busq.argum[5])
	dw_1.SetItem(il_fila, "pafr_calrot",ls_null)
	dw_1.SetColumn("pafr_varrot")
	dw_1.SetFocus()
END IF

end subroutine

public subroutine buscaproductor ();Str_busqueda	lstr_busq

dw_1.Modify("buscaproductor.border = 0")
dw_1.Modify("buscaproductor.border = 5")

istr_busq.argum[1]	=	istr_mant.Argumento[1]

OpenWithParm(w_busc_productores_clientes, istr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] <> '' THEN
	istr_mant.argumento[3]	=	lstr_busq.argum[3]
	istr_mant.argumento[4]	=	lstr_busq.argum[4]
	
	dw_1.setItem(il_fila, "pafr_prdrot", Integer(lstr_busq.argum[3]))
	dw_1.setItem(il_fila, "prod_nombre_1", lstr_busq.argum[4])
	dw_1.SetColumn("pafr_prdrot")
	dw_1.SetFocus()
END IF





end subroutine

public function boolean existeembalaje (string as_embalaje, integer al_cliente);Integer	li_Existes
Boolean	lb_Retorno


SELECT Count(*)
INTO	 :li_Existes
FROM	 dba.embalajesprod
WHERE  emba_codigo	=	:as_embalaje
AND	clie_codigo = :al_cliente;

IF sqlca.sqlcode	=	-1	THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de embalajesprod")
ELSEIF 	li_Existes	=	0	THEN
	MessageBox("Atención","Código de Embalaje no Existe.~r " +&
	"Ingrese Otro.")			
	lb_Retorno	=	TRUE
END IF


RETURN lb_Retorno
end function

event open;call super::open;ii_tipo	=	Integer(Message.StringParm)

dw_3.SetTransObject(sqlca)
dw_3.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_3.InsertRow(0)
dw_3.SetItem(1,"clie_codigo", gi_codexport)

dw_4.SetTransObject(sqlca)
dw_4.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_4.InsertRow(0)
dw_4.SetItem(1,"plde_codigo", gi_codplanta)

istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)



end event

on w_mant_mantenerotula.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_1=create st_1
this.em_nroguia=create em_nroguia
this.st_3=create st_3
this.dw_3=create dw_3
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.em_nroguia
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.dw_3
this.Control[iCurrent+6]=this.dw_4
end on

on w_mant_mantenerotula.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.em_nroguia)
destroy(this.st_3)
destroy(this.dw_3)
destroy(this.dw_4)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(long(istr_mant.argumento[6]),integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))
	
	IF ll_fila	= -1 THEN
		respuesta	= MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta	 = 1

IF respuesta	= 2 THEN 
	Close(This)
ELSE	
	pb_insertar.Enabled = True
END IF















//Long	ll_fila, ll_fila1, respuesta
//
//ii_nuevo	=	0
//
//DO
//	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]))
//	IF ll_fila = -1 THEN
//		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
//										Information!, RetryCancel!)
//	ELSE
//		ll_fila1	=	dw_2.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),&
//									 Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]))
//		IF ll_fila1 > 0 THEN ii_nuevo	=	1
//		dw_1.SetRow(1)
//		dw_1.SetFocus()
//		pb_eliminar.Enabled	=	True
//		pb_grabar.Enabled		=	True
//		pb_imprimir.Enabled	=	True
//		pb_insertar.SetFocus()
//	END IF
//LOOP WHILE respuesta = 1
//
//IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir;//SetPointer(HourGlass!)
//
//Long		ll_Filas
//str_info	lstr_info
//
//lstr_info.titulo	= "SOLICITUD INSPECCION FITOSANITARIA S.A.G."
//lstr_info.copias	= 1
//
//OpenWithParm(vinf,lstr_info)
//
//vinf.dw_1.DataObject = "dw_info_solicitud_inspeccion_compuesto"
//
//vinf.dw_1.SetTransObject(sqlca)
//
//ll_Filas	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]), &
//											Long(istr_mant.argumento[4]), &
//											Integer(istr_mant.argumento[1]), &
//											Integer(istr_mant.argumento[2]))
//
//IF ll_Filas = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF ll_Filas = 0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	vinf.dw_1.Object.dw_detalle.Object.nom_empresa.text	= gstr_apl.nom_empresa
//	vinf.dw_1.Object.dw_detalle.Object.rut_empresa.text = 'R.U.T. ' + String(Double(Mid(gstr_apl.rut_empresa,1,9)),'###,###,###') + '-' + Mid(gstr_apl.rut_empresa,10,1)
//	vinf.dw_1.Object.dw_detalle.Object.dir_empresa.text	= gstr_apl.dir_empresa
//	
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//
//SetPointer(Arrow!)
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

		
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas Registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF
	
	IF IsNull(dw_1.Object.pafr_varrot[il_fila]) OR dw_1.Object.pafr_varrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad"
		ls_colu[li_cont]	= "pafr_varrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_prdro[il_fila]) OR dw_1.Object.pafr_prdro[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Productor"
		ls_colu[li_cont]	= "pafr_prdro"
	END IF
	
	IF IsNull(dw_1.Object.pafr_calrot[il_fila]) OR dw_1.Object.pafr_calrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta el ingreso del Calibre"
		ls_colu[li_cont]	= "pafr_calrot"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
		ii_contador = 1
		
		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
			ii_contador++
		loop 
		ii_contador --
		dw_1.SetRow(ii_contador)
		Message.DoubleParm = -1
	END IF
END IF





end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

		
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas Registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF
	
	IF IsNull(dw_1.Object.pafr_varrot[il_fila]) OR dw_1.Object.pafr_varrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad"
		ls_colu[li_cont]	= "pafr_varrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_prdrot[il_fila]) OR dw_1.Object.pafr_prdrot[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Productor"
		ls_colu[li_cont]	= "pafr_prdrot"
	END IF
	
	IF IsNull(dw_1.Object.pafr_calrot[il_fila]) OR dw_1.Object.pafr_calrot[il_fila] = ""	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nFalta el ingreso del Calibre"
		ls_colu[li_cont]	= "pafr_calrot"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
//		ii_contador = 1
		
//		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
//			ii_contador++
//		loop 
//		ii_contador --
//		dw_1.SetRow(ii_contador)
//		Message.DoubleParm = -1
	END IF
END IF





end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mantenerotula
integer y = 60
integer width = 4466
integer height = 416
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mantenerotula
integer x = 4626
integer y = 496
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;dw_3.Enabled			=	TRUE
dw_4.Enabled			=	TRUE
em_nroguia.Enabled	=	TRUE

dw_3.Reset()
dw_3.InsertRow(0)
dw_3.SetFocus()
dw_3.Object.clie_codigo.Background.Color=	RGB(255, 255, 255)
dw_3.SetItem(1,"clie_codigo", gi_codexport)

dw_4.Reset()
dw_4.InsertRow(0)
dw_4.Object.plde_codigo.Background.Color=	RGB(255, 255, 255)
dw_4.SetItem(1,"plde_codigo", gi_codplanta)

em_nroguia.BackColor=	RGB(255, 255, 255)
em_nroguia.text = ""



istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mantenerotula
integer x = 4626
integer y = 200
integer taborder = 50
end type

event pb_lectura::clicked;integer li_nombrevari


IF IsNull(dw_3.Object.clie_codigo[1]) OR dw_3.Object.clie_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Cliente Previamente",Exclamation!)
	RETURN
ELSEIF IsNull(dw_4.Object.plde_codigo[1]) OR  dw_4.Object.plde_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Planta Previamente",Exclamation!)
	RETURN
ELSEIF IsNull(integer(em_nroguia.text)) OR integer(em_nroguia.text) = 0 THEN
	MessageBox("Atención","Debe Seleccionar Nº de Guía Previamente",Exclamation!)
	RETURN
	em_nroguia.SetFocus()
ELSE
	dw_3.Enabled	=	FALSE
	dw_4.Enabled	=	FALSE
	em_nroguia.Enabled	=	FALSE
	dw_3.Object.clie_codigo.Background.Color=	RGB(166,180,210)
	dw_4.Object.plde_codigo.Background.Color=	RGB(166,180,210)
	em_nroguia.BackColor=	RGB(166,180,210)
	Parent.PostEvent("ue_recuperadatos")
	
END IF	
		
	

		
			
	



end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mantenerotula
boolean visible = false
integer x = 4626
integer y = 856
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mantenerotula
boolean visible = false
integer x = 4626
integer y = 676
integer taborder = 80
end type

event pb_insertar::clicked;//
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_mantenerotula
integer x = 4626
integer y = 1600
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mantenerotula
boolean visible = false
integer x = 4626
integer y = 1216
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mantenerotula
integer x = 4626
integer y = 1036
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mantenerotula
integer y = 496
integer width = 4466
integer height = 1276
integer taborder = 60
string dataobject = "dw_mues_mantrotula"
boolean hscrollbar = true
end type

event dw_1::clicked;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
END IF

	
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF
	
	IF IsNull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0	THEN
	END IF
	
	IF IsNull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0	THEN
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
		ii_contador = 1
		
		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
			ii_contador++
		loop 
		ii_contador --
		dw_1.SetRow(ii_contador)
		Message.DoubleParm = -1
	END IF
END IF

IF dw_1.rowcount() > 0 THEN
	IF dw_1.Object.Paen_numero[il_fila] = 0 THEN
		dw_1.SetRow(ii_contador)
	END IF
END IF



//CHOOSE CASE ls_Columna	
//		
//	CASE "pafr_varrot"
//		istr_mant.Argumento[2]	=	Data	
//			dw_1.SetItem(il_fila,"vari_nombre_1",f_variedadnom(integer(data)))
//			dw_1.SetItem(il_fila,"pafr_calrot",ls_null)
//			IF NoExisteVariedad(ls_Columna,Data)	THEN
//			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
//			RETURN 1
//		END IF
//	
//END CHOOSE
//
//
//
//
//
end event

event dw_1::itemchanged;call super::itemchanged;String ls_Columna, ls_null, ls_varnom
Integer	li_Nula 


SetNull(li_Nula)
SetNull(ls_null)

dw_1.AcceptText()

ls_Columna	=	Dwo.Name

CHOOSE CASE ls_Columna	
		
	CASE "pafr_varrot"
		istr_mant.Argumento[2]	=	Data	
			dw_1.SetItem(il_fila,"vari_nombre_1",f_variedadnom(integer(data)))
			dw_1.SetItem(il_fila,"pafr_calrot",ls_null)
			IF NoExisteVariedad(ls_Columna,Data)	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
			RETURN 1
			
	END IF
		
	CASE "pafr_prdrot"
		istr_mant.Argumento[3]	=	Data
		dw_1.SetItem(il_fila,"prod_nombre_1",f_prodnombre(integer(data)))
		IF existeproductor(Data)	THEN
			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)			
			RETURN 1
		END IF
 
   CASE "pafr_calrot"
    IF len(data) = 1 then
		istr_mant.Argumento[4]	=	string(Data) + "  "
	 ELSEIF  len(data) = 2 then
		istr_mant.Argumento[4]	=	string(Data) + " "
	 ELSE
		istr_mant.Argumento[4]	=	string(Data)
	 END IF
	
	   IF existecalibre(istr_mant.Argumento[4])	THEN
			dw_1.SetItem(il_Fila,ls_Columna,ls_Null)			
			RETURN 1
		END IF
		
	CASE 'emba_codigo'
		IF existeembalaje(Data,dw_3.Object.clie_codigo[1])	THEN
			dw_1.SetItem(il_Fila,'emba_codigo',String(li_Nula))
			RETURN 1
			
		END IF	
	
	END CHOOSE	




end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.name		
	CASE "buscavariedad"
			buscavariedad()
	
	CASE "buscaproductor"
		buscaproductor()		
END CHOOSE


end event

event dw_1::rowfocuschanged;integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::dwnkey;This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		ELSEIF Key = KeyDownArrow! AND il_fila = dw_1.RowCount() THEN
			Parent.TriggerEvent("ue_nuevo")
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			ELSE
				Parent.TriggerEvent("ue_nuevo")
				
				This.SetFocus()
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

type st_2 from statictext within w_mant_mantenerotula
integer x = 169
integer y = 228
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_mantenerotula
integer x = 169
integer y = 112
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type em_nroguia from editmask within w_mant_mantenerotula
integer x = 507
integer y = 328
integer width = 402
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[6]=em_nroguia.text

IF ExisteGuia(Long(em_nroguia.text)) = False THEN
	This.SetFocus()
END IF
end event

type st_3 from statictext within w_mant_mantenerotula
integer x = 169
integer y = 344
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Nro. Guia"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_mant_mantenerotula
integer x = 507
integer y = 96
integer width = 1230
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	String(data)
//idwc_planta.Retrieve(Integer(istr_mant.argumento[1]),1)
//istr_mant.argumento[2]	=	String(dw_1.Object.plde_codigo[1])
//dw_1.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
end event

type dw_4 from datawindow within w_mant_mantenerotula
integer x = 507
integer y = 212
integer width = 965
integer height = 96
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]=String(data)
end event

