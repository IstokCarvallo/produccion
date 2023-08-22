$PBExportHeader$w_mant_deta_sagcambiodestdeta.srw
forward
global type w_mant_deta_sagcambiodestdeta from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_sagcambiodestdeta from w_mant_detalle_csd
integer width = 2496
integer height = 1312
end type
global w_mant_deta_sagcambiodestdeta w_mant_deta_sagcambiodestdeta

type variables
Integer ii_estado

DataWindowChild	dw_destino

uo_destinos       iuo_destinos
end variables

forward prototypes
public function boolean duplicado (long al_numero)
public function boolean noexistepallet (long al_numero)
public subroutine buscapallet ()
end prototypes

public function boolean duplicado (long al_numero);Long		ll_fila
Integer	li_cliente, li_planta, li_cantid

ll_fila	=	dw_1.Find("clie_codigo = " + istr_mant.Argumento[3] + &
						" AND plde_codigo = " + istr_mant.Argumento[1] + &
						" AND paen_numero = " + String(al_Numero) , 1, dw_1.RowCount())
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Pallet ya fue incluido en Cambio de Destino", Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

public function boolean noexistepallet (long al_numero);String	ls_nomvar, ls_embala
Integer	li_catego, li_Cliente, li_ContCalidad, li_tipopa, li_cajas, li_inspec, &
			li_Destino, li_Especie, li_Variedad, li_Planta, li_fumiga
Long 		ll_Pallet, ll_num_inspec

li_cliente	= 	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])

SELECT	pae.paen_tipopa, var.vari_nombre, pae.emba_codigo, pae.cate_codigo,
			pae.paen_concal, pae.paen_ccajas, pae.paen_inspec, pae.dest_codigo,
			pae.espe_codigo, pae.vari_codigo, pae.paen_estado, pae.cond_codigo
	INTO	:li_tipopa, :ls_nomvar, :ls_embala, :li_catego, :li_ContCalidad, :li_cajas,
			:li_inspec, :li_Destino, :li_Especie, :li_Variedad, :ii_estado, :li_fumiga
	FROM	dba.palletencab as pae, dba.variedades as var
	WHERE pae.clie_codigo	= 	:li_cliente
	AND	pae.paen_numero	= 	:al_numero
	AND	pae.plde_codigo	=	:li_planta
	AND	var.espe_codigo	= 	pae.espe_codigo
	AND	var.vari_codigo	= 	pae.vari_codigo ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Pallet no ha sido creado, Ingrese otro Código.", &
			Exclamation!, OK!)
	RETURN True
ELSE
	/*	Verifica Condición de Inspección	*/
	IF (IsNull(li_inspec) OR li_inspec = 0)  THEN
		MessageBox("ATENCION", "Este Pallet no ha sido INSPECCIONADO .", &
				Exclamation!, OK!)
		RETURN TRUE
	ELSE
		dw_1.Object.sagd_desant[il_fila] = li_destino	
		RETURN FALSE
	END IF
END IF

end function

public subroutine buscapallet ();Long    ll_Pallet, ll_num_inspec
Integer li_planta, li_cliente
Date    ld_fecha    

dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 5")

istr_busq.argum[2]	=	""
istr_busq.argum[1]	=	String(dw_1.Object.clie_codigo[il_fila])
istr_busq.argum[5]	=	String(dw_1.Object.plde_codigo[il_fila])
istr_busq.argum[3]	=	"1"

OpenWithParm(w_busc_palletencab, istr_busq)

istr_busq	       	=	Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	
	//NoExistePallet(Long(istr_busq.Argum[2]))
	
	IF NoExistePallet(Long(istr_busq.Argum[2])) OR NOT Duplicado(Long(istr_busq.Argum[2])) THEN
		ll_pallet  = Long(Istr_Busq.argum[2])
		li_Cliente = Integer(istr_busq.argum[1])
		li_Planta  = Integer(istr_busq.argum[7])
	
		/* Selecciona el último número de inspección del Pallet */	
										
		SELECT Max(inpd_fechai) 
		  INTO :ld_fecha
		  FROM dba.inspecpaldet 	
		 WHERE clie_codigo = :li_Cliente AND  
				 plde_codigo = :li_Planta  AND  
				 paen_numero = :ll_Pallet; 
				 
		SELECT inpe_numero 
		  INTO :ll_num_inspec  
		  FROM dba.inspecpaldet 	AS i 
		 WHERE clie_codigo = :li_Cliente AND  
				 plde_codigo = :li_Planta  AND  
				 paen_numero = :ll_Pallet  AND
				 inpd_fechai = :ld_fecha; 		 
				 
				 
		//dw_1.Object.sagd_desant[il_fila] = Integer(Istr_Busq.argum[5])
		
		dw_1.Object.paen_numero[il_fila] = ll_Pallet
		dw_1.Object.sagd_inspec[il_fila] = ll_num_inspec
		dw_1.SetColumn("paen_numero")
	   dw_1.SetFocus()
	END IF
END IF

dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 6")

RETURN
end subroutine

on w_mant_deta_sagcambiodestdeta.create
call super::create
end on

on w_mant_deta_sagcambiodestdeta.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;dw_1.modify("buscapallet.Visible=1")

ias_campo[1]	=	String(dw_1.Object.paen_numero[il_fila])
ias_campo[2]	=	String(dw_1.Object.dest_codigo[il_fila])
ias_campo[3]	=	String(dw_1.Object.sagd_desant[il_fila])
ias_campo[4]	=	String(dw_1.Object.sagd_inspec[il_fila])

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "sagc_numero", Long(istr_mant.argumento[2]))
END IF

IF istr_mant.agrega = False and istr_mant.borra = False THEN
		dw_1.SetTabOrder("paen_numero", 0)
		dw_1.Modify("paen_numero.BackGround.Color = " + String(RGB(166,180,210)))
		dw_1.modify("buscapallet.Visible=0")
END IF

end event

event ue_deshace;call super::ue_deshace;dw_1.Object.paen_numero[il_fila]	=	Long(ias_campo[1])
dw_1.Object.dest_codigo[il_fila]	=	Long(ias_campo[2])
dw_1.Object.sagd_desant[il_fila]	=	Long(ias_campo[3])
dw_1.Object.sagd_inspec[il_fila]	=	Long(ias_campo[4])
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.paen_numero[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNúmero de Pallet"
	ls_colu[li_cont]	= "paen_numero"
END IF

IF Isnull(dw_1.Object.dest_codigo[il_fila]) OR dw_1.Object.dest_codigo[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNuevo Destino"
	ls_colu[li_cont]	= "dest_codigo"
END IF

IF Isnull(dw_1.Object.sagd_desant[il_fila]) OR dw_1.Object.sagd_desant[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nDestino Anterior"
	ls_colu[li_cont]	= "sagd_desant"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus() 
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;ib_ok = True

This.TriggerEvent("ue_guardar")
IF Message.DoubleParm = -1 THEN ib_ok = False

IF ib_ok = False THEN RETURN

wf_nuevo()
dw_1.SetItem(il_fila, "plde_codigo",  Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "sagc_numero",  Long(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "clie_codigo",  Integer(istr_mant.argumento[3]))

dw_1.SetColumn("paen_numero")
dw_1.SetFocus()

end event

event open;call super::open;/*
	Argumentos	:	[1]	=	Código de Planta
						[2]	=	Número de Folio 
						[3]	=	Código de Cliente
					
*/

iuo_destinos      = CREATE uo_destinos       

dw_1.GetChild("dest_codigo", dw_destino)
dw_destino.SetTransObject(sqlca)
IF dw_destino.Retrieve(1) = 0 THEN
	dw_destino.insertRow(0)
END IF

dw_1.GetChild("sagd_desant", dw_destino)
dw_destino.SetTransObject(sqlca)
IF dw_destino.Retrieve() = 0 THEN
	dw_destino.insertRow(0)
END IF

end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_sagcambiodestdeta
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_sagcambiodestdeta
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_sagcambiodestdeta
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_sagcambiodestdeta
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_sagcambiodestdeta
integer x = 2199
integer y = 352
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_sagcambiodestdeta
integer x = 2199
integer y = 136
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_sagcambiodestdeta
integer x = 2199
integer y = 568
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_sagcambiodestdeta
integer height = 968
string dataobject = "dw_mant_sagcambiodestdeta"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_Nula
Integer	li_cliente,pallet,li_status,li_tipo, li_Planta
Date     ld_Fecha
Long     ll_Pallet, ll_num_inspec  

SetNull(ls_Nula)

ls_columna = dwo.name

CHOOSE CASE ls_columna
			
	CASE "paen_numero"
		
		IF NoExistePallet(Long(data)) OR Duplicado(Long(data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			RETURN 1
		ELSE
			ll_pallet  = Long(data)
			li_Cliente = Integer(istr_mant.argumento[3])
			li_Planta  = Integer(istr_mant.argumento[1])
		
			/* Selecciona el último número de inspección del Pallet */	
											
			SELECT Max(inpd_fechai) 
			  INTO :ld_fecha
			  FROM dba.inspecpaldet 	
			 WHERE clie_codigo = :li_Cliente AND  
					 plde_codigo = :li_Planta  AND  
					 paen_numero = :ll_Pallet; 
					 
			SELECT inpe_numero 
			  INTO :ll_num_inspec  
			  FROM dba.inspecpaldet 	AS i 
			 WHERE clie_codigo = :li_Cliente AND  
					 plde_codigo = :li_Planta  AND  
					 paen_numero = :ll_Pallet  AND
					 inpd_fechai = :ld_fecha; 		 
					 
			dw_1.Object.paen_numero[il_fila] = ll_Pallet
			dw_1.Object.sagd_inspec[il_fila] = ll_num_inspec
			dw_1.SetColumn("paen_numero")
			dw_1.SetFocus()
		END IF
		
		
	CASE "dest_codigo"	
				
		IF NOT iuo_destinos.existe(Integer(data),TRUE, sqlca) THEN
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			RETURN 1
		END IF

END CHOOSE
end event

event dw_1::clicked;call super::clicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "buscapallet"
		BuscaPallet()
		
END CHOOSE
end event

