$PBExportHeader$w_mant_instructivo_emb.srw
forward
global type w_mant_instructivo_emb from w_mant_encab_deta
end type
type cb_2 from uo_cbstandard within w_mant_instructivo_emb
end type
type cb_3 from uo_cbstandard within w_mant_instructivo_emb
end type
type cb_4 from uo_cbstandard within w_mant_instructivo_emb
end type
type dw_enca from uo_dw within w_mant_instructivo_emb
end type
type dw_pallets from uo_dw within w_mant_instructivo_emb
end type
type cb_agrega from uo_cbstandard within w_mant_instructivo_emb
end type
type cb_elimina from uo_cbstandard within w_mant_instructivo_emb
end type
type dw_4 from uo_dw within w_mant_instructivo_emb
end type
type dw_3 from uo_dw within w_mant_instructivo_emb
end type
type dw_5 from datawindow within w_mant_instructivo_emb
end type
type dw_6 from uo_dw within w_mant_instructivo_emb
end type
end forward

global type w_mant_instructivo_emb from w_mant_encab_deta
integer width = 3918
integer height = 2300
string title = "SELECCION DE PALLETS"
windowstate windowstate = maximized!
cb_2 cb_2
cb_3 cb_3
cb_4 cb_4
dw_enca dw_enca
dw_pallets dw_pallets
cb_agrega cb_agrega
cb_elimina cb_elimina
dw_4 dw_4
dw_3 dw_3
dw_5 dw_5
dw_6 dw_6
end type
global w_mant_instructivo_emb w_mant_instructivo_emb

type variables
long clie_codigo, il_filaactual,ii_dese_numero, il_total
integer  ii_tipo, ii_cliente, ii_planta
String ii_embqcod, is_recibidor, is_nave, is_puerto, is_destino, is_tipotr, is_fzarpe, is_obs1, is_obs2
datastore ids_despaselecenca, ids_despaselecdeta, ids_despaselectsele
dataWindowChild idwc_planta, idwc_cliente, idwc_destino, idwc_status, idwc_secuencia, idwc_productor

boolean ib_entraporbusqueda = false, ib_AutoCommit

uo_ProdCuarteles	iuo_Cuartel
uo_Productores		iuo_Productor
uo_camarasbode	iuo_Camara
uo_variedades		iuo_variedad
uo_especie			iuo_expecie
end variables

forward prototypes
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean buscaembarque ()
public function long buscanuevofolio (integer planta)
public function boolean existedespa (long ai_desenumero, integer ai_cliente, integer ai_planta)
public function string nombre_planta (integer ai_planta)
public function string nombre_cliente (integer ai_cliente)
public function boolean datos_embarque (string as_embarque)
public subroutine busca_observaciones (integer ai_cliente, integer ai_planta, long ai_desenumero)
public subroutine wf_activa_filtros_dw1 (string fs_filtro, string fs_dato, string fs_dato2)
end prototypes

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno
Long		Numero, ll_numero, ll_fil, ll_fila
Integer	li_planta, li_movto, li_fila

ii_dese_numero = dw_enca.Object.dese_numero[1]

IF il_total = 0 THEN
	FOR ll_fila = 1 TO dw_pallets.RowCount() 
		 li_fila = dw_5.Retrieve(dw_enca.Object.Clie_Codigo[1],dw_enca.Object.Plde_Codigo[1],dw_pallets.Object.paen_numero[ll_fila],&
				ii_tipo,dw_pallets.Object.espe_codigo[ll_fila],dw_pallets.Object.vari_codigo[ll_fila],&
				dw_pallets.Object.pafr_calibr[ll_fila])
		For ll_numero = 1 to dw_5.Rowcount()
				dw_5.DeleteRow(ll_numero)
				IF dw_5.Update(True, False) = 1 THEN			//despaselecsele
					Commit;
					IF sqlca.SQLCode <> 0 THEN
						
						RollBack;
					ELSE
						dw_5.ResetUpdate()
					END IF
				
				END IF
		NEXT
	NEXT	
END IF

ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_pallets.Update(True, False) = 1 THEN 					//despaselecenca
		IF dw_1.Update(True, False) = 1 THEN						//despaselecdeta
			IF dw_enca.Update(True, False) = 1 THEN				//despaselecsele
				Commit;
						
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				ELSE
					lb_Retorno	=	True
				
					dw_enca.ResetUpdate()
					dw_1.ResetUpdate()
					dw_pallets.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_enca.Update(True, False) = 1 THEN		 				//despaselecenca
		IF dw_1.Update(True, False) = 1 THEN						//despaselecdeta
			IF dw_pallets.Update(True, False) = 1 THEN			//despaselecsele
				Commit;
		
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
				
					dw_enca.ResetUpdate()
					dw_1.ResetUpdate()
					dw_pallets.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
END IF


sqlca.AutoCommit	=	ib_AutoCommit

Numero = Long(istr_busq.argum[5])
li_planta = Integer(istr_busq.argum[3])
li_movto = 9
il_total = 0

/*actualiza numero actual en correlativos */
update dbo.CORRELMOVIMIENTOS set
como_actual = :numero
where plde_codigo = :li_planta
And	como_tipomv = :li_movto;

This.TriggerEvent("ue_imprimir")
This.TriggerEvent("ue_nuevo")

RETURN lb_Retorno
end function

public function boolean buscaembarque ();long ll_filas, ll_i

dw_2.SetTransObject(SQLCA)
ll_filas = dw_2.retrieve(long(istr_busq.argum[1]),istr_busq.argum[2])

IF ll_filas = 0 THEN
	Messagebox("Atencion", "No Existen datos para el Codigo de Embarque", Exclamation!)
	return false
ELSEIF ll_filas < 0 THEN
	return false
ELSE
	FOR ll_i = 1 TO ll_filas
		IF IsNull(dw_2.object.embq_fzarpe[ll_i]) THEN
			dw_2.DeleteRow(ll_i)
		END IF
	NEXT
	IF dw_2.RowCount() > 0 THEN
//		pb_ins_det.Enabled = TRUE
//		pb_eli_det.Enabled = TRUE
//		pb_grabar.Enabled = TRUE
		RETURN TRUE
	ELSE
		RETURN FALSE
	END IF
END IF
end function

public function long buscanuevofolio (integer planta);Integer	li_planta, li_tipoins, li_movto
Long		ll_numero,ll_numero2, ll_actual, ll_fin
Boolean	lb_nulo

li_planta	=	planta


IF ii_tipo = 1 THEN
	li_movto = 9
	Select max(dese_numero) 
	Into  :ll_numero
	From dbo.despaselecenca
	Where plde_codigo = :li_planta
	AND   dece_tipsel = 1;
ELSE
	li_movto = 10
	Select max(dese_numero) 
	Into  :ll_numero
	From dbo.despaselecenca
	Where plde_codigo = :li_planta
	AND   dece_tipsel = 2;
END IF	

Select como_inicia, como_actual, como_termin
Into	:ll_numero2, :ll_actual, :ll_fin
from dbo.correlmovimientos
Where plde_codigo = :li_planta
and	como_tipomv = :li_movto;

IF ll_actual >= ll_fin THEN
	Return 0
END IF	

ll_fin = ll_fin - 3

IF ll_actual >= ll_fin THEN 
	MessageBox("Advertencia","Quedan Menos de 3 Correlativos, proceda por mantención 'Correlativos'")
END IF	

IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla CORRELMOVIMIENTOS")
END IF

IF Isnull(ll_numero) OR String(ll_numero) = '' or ll_numero < ll_numero2 THEN
	ll_numero = ll_numero2
END IF	

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla despaselecenca")
ELSEIF sqlca.SQLCode = 0 THEN
	 ll_numero++
END IF

RETURN ll_numero

end function

public function boolean existedespa (long ai_desenumero, integer ai_cliente, integer ai_planta);String ls_embqcod

SetNull(ls_embqcod)

Select embq_codigo,dese_obser1,dese_obser2
into :ls_embqcod,:is_obs1,:is_obs2
from dbo.despaselecenca as de
where de.clie_codigo = :ai_cliente
and 	de.plde_codigo = :ai_planta
and 	de.dese_numero = :ai_desenumero
and	de.dece_tipsel = :ii_tipo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Despaselecenca")
	
	RETURN false
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Numero de despacho no ha sido creado.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN false
ELSE
	ii_embqcod = ls_embqcod
	RETURN true
END IF
end function

public function string nombre_planta (integer ai_planta);String ls_nombre

Select plde_nombre
into :ls_nombre
from dbo.plantadesp 
where	plde_codigo = :ai_planta;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla plantadesp")
	
	RETURN ls_nombre
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Numero de Planta no ha sido creada.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN ls_nombre
ELSE

	RETURN ls_nombre
END IF
end function

public function string nombre_cliente (integer ai_cliente);String ls_nombre

Select clie_nombre
into :ls_nombre
from dbo.clientesprod 
where	clie_codigo = :ai_cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Clientesprod")
	
	RETURN ls_nombre
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Numero de Cliente no ha sido creada.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN ls_nombre
ELSE

	RETURN ls_nombre
END IF
end function

public function boolean datos_embarque (string as_embarque);String 	ls_nombre, ls_tipotr
Integer	li_recibidor, li_nave, li_origen, li_puerto
Long		ll_Cliente
Date		ld_fecha

ll_Cliente	=	dw_enca.Object.clie_codigo[1]

SELECT embq_clifac,nave_codigo,embq_fzarpe,embq_ptoori,puer_codigo,nave_tipotr
INTO :li_recibidor,:li_nave,:ld_fecha,:li_origen,:li_puerto,:ls_tipotr
FROM dbo.embarqueprod 
WHERE clie_codigo = :ll_Cliente
	AND embq_codigo = :as_embarque;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla EmbarquesProd")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Numero de Embarque no ha sido creada.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	SELECT cons_nombre
	INTO :is_recibidor
	FROM dbo.consignatario
	WHERE cons_codigo = :li_recibidor;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de tabla consignatario")
		
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Numero de Consignatario no ha sido creada.~r~r" + &
					"Ingrese o seleccione otro Código.")
		RETURN True
	END IF
	
	SELECT nave_nombre
	INTO :is_nave
	FROM dbo.naves
	WHERE nave_codigo = :li_nave
	AND	nave_tipotr = :ls_tipotr;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de tabla naves")
		
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Numero de nave no ha sido creada.~r~r" + &
					"Ingrese o seleccione otro Código.")
		RETURN True
	END IF
	
	SELECT puer_nombre
	INTO :is_puerto
	FROM dbo.puertos
	WHERE puer_codigo = :li_origen;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de tabla puertos")
		
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de puertos no ha sido creada.~r~r" + &
					"Ingrese o seleccione otro Código.")
		RETURN True
	END IF
	
	SELECT puer_nombre
	INTO :is_destino
	FROM dbo.puertos
	WHERE puer_codigo = :li_puerto;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de tabla puertos")
		
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de puertos no ha sido creada.~r~r" + &
					"Ingrese o seleccione otro Código.")
		RETURN True
	END IF
	is_fzarpe = String(ld_fecha)
	RETURN False
END IF
end function

public subroutine busca_observaciones (integer ai_cliente, integer ai_planta, long ai_desenumero);String ls_embqcod

Select dese_obser1,dese_obser2
into :is_obs1,:is_obs2
from dbo.despaselecenca as de
where de.clie_codigo = :ai_cliente
and 	de.plde_codigo = :ai_planta
and 	de.dese_numero = :ai_desenumero
and	de.dece_tipsel = :ii_tipo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Despaselecenca")
END IF
end subroutine

public subroutine wf_activa_filtros_dw1 (string fs_filtro, string fs_dato, string fs_dato2);datawindowchild dwc_especie, dwc_embalaje, dwc_etiqueta, dwc_variedad, dwc_calibre, dwc_productor
integer li_ret

CHOOSE CASE fs_filtro
	CASE "especie"
		li_ret = dw_1.GetChild("espe_codigo", dwc_especie)
		IF li_ret > 0 THEN
			dwc_especie.SetTransObject(SQLCA)
			dwc_especie.Retrieve()
			
			dwc_variedad.SetTransObject(SQLCA)
			dwc_variedad.Retrieve(Integer(fs_dato))
			dwc_Variedad.InsertRow(1)
			
			dwc_Embalaje.SetTransObject(SQLCA)
			dwc_Embalaje.Retrieve(dw_enca.Object.clie_codigo[1], Integer(fs_dato))
			dwc_Embalaje.InsertRow(1)
					
		END IF
		
	CASE "variedad"
		li_ret = dw_1.GetChild("vari_codigo", dwc_variedad)
		IF li_ret > 0 THEN
			dwc_variedad.SetTransObject(SQLCA)
			dwc_variedad.Retrieve(Integer(fs_dato))
			dwc_Variedad.InsertRow(1)
			
			dwc_Variedad.SetItem(1, "vari_codigo", -1)
			dwc_Variedad.SetItem(1, "vari_nombre", "TODOS")
		END IF
		
	CASE "embalaje"
		li_ret = dw_1.GetChild("emba_codigo", dwc_embalaje)
		IF li_ret > 0 THEN
			dwc_Embalaje.SetTransObject(SQLCA)
			dwc_Embalaje.Retrieve(dw_enca.Object.clie_codigo[1], Integer(fs_dato))
			dwc_Embalaje.InsertRow(1)
			
			dwc_Embalaje.SetItem(1, "emba_codigo", "*")
			dwc_Embalaje.SetItem(1, "emba_nombre", "TODOS")
		END IF
		
	CASE "etiqueta"
		li_ret = dw_1.GetChild("etiq_codigo", dwc_etiqueta)
		IF li_ret > 0 THEN
			dwc_etiqueta.SetTransObject(SQLCA)
			dwc_etiqueta.Retrieve()
			dwc_etiqueta.InsertRow(1)
			
			dwc_etiqueta.SetItem(1, "etiq_codigo", -1)
			dwc_etiqueta.SetItem(1, "etiq_nombre", "TODOS")
		END IF
		
	CASE "calibre"
		li_ret = dw_1.GetChild("paen_calibr", dwc_calibre)
		IF li_ret > 0 THEN
			dwc_calibre.SetTransObject(SQLCA)
			dwc_calibre.Retrieve()
			dwc_calibre.InsertRow(1)
			
			dwc_calibre.SetItem(1, "caen_calibr", 'TOD')
		END IF
		
	CASE "productor"
		li_ret = dw_1.GetChild("prod_codigo", dwc_productor)
		IF li_ret > 0 THEN
			dwc_productor.SetTransObject(SQLCA)
			dwc_productor.Retrieve(Integer(istr_busq.argum[1]))
			dwc_productor.InsertRow(1)
			
			dwc_productor.SetItem(1, "prod_nombre", 'TODOS')
			dwc_productor.SetItem(1, "prod_codigo", -1)
		END IF
		
		
	CASE "destino"
		li_ret = dw_1.GetChild("dest_codigo", idwc_destino)
		IF li_ret > 0 THEN
			idwc_destino.SetTransObject(SQLCA)
			idwc_destino.Retrieve()
			idwc_destino.InsertRow(1)
			
			idwc_destino.SetItem(1, "dest_nombre", 'TODOS')
			idwc_destino.SetItem(1, "dest_codigo", -1)
			//dw_1.Object.dese_secdet.Protect = 1
		ELSE
			dw_1.Object.dese_secdet.Protect = 0
					
		END IF
		
		dw_1.GetChild("dese_secdet", idwc_secuencia)
		idwc_secuencia.SetTransObject(sqlca)
		idwc_secuencia.Retrieve(Integer(fs_dato),integer(fs_dato2))
		idwc_secuencia.insertRow(0)
		
		
	CASE "secuencia"
		
		dw_1.GetChild("dese_secdet", idwc_secuencia)
		idwc_secuencia.SetTransObject(sqlca)
		IF idwc_secuencia.Retrieve(Integer(fs_dato),integer(fs_dato2)) = 0 THEN
			idwc_secuencia.insertRow(0)
		END IF
			
	CASE "status"
		li_ret = dw_1.GetChild("stat_codigo", idwc_status)
		IF li_ret > 0 THEN
			idwc_status.SetTransObject(SQLCA)
			idwc_status.Retrieve()
			idwc_status.InsertRow(1)
			
			idwc_status.SetItem(1, "stat_nombre", 'TODOS')
			idwc_status.SetItem(1, "stat_codigo", -1)
				
		END IF	
		
END CHOOSE
end subroutine

on w_mant_instructivo_emb.create
int iCurrent
call super::create
this.cb_2=create cb_2
this.cb_3=create cb_3
this.cb_4=create cb_4
this.dw_enca=create dw_enca
this.dw_pallets=create dw_pallets
this.cb_agrega=create cb_agrega
this.cb_elimina=create cb_elimina
this.dw_4=create dw_4
this.dw_3=create dw_3
this.dw_5=create dw_5
this.dw_6=create dw_6
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_2
this.Control[iCurrent+2]=this.cb_3
this.Control[iCurrent+3]=this.cb_4
this.Control[iCurrent+4]=this.dw_enca
this.Control[iCurrent+5]=this.dw_pallets
this.Control[iCurrent+6]=this.cb_agrega
this.Control[iCurrent+7]=this.cb_elimina
this.Control[iCurrent+8]=this.dw_4
this.Control[iCurrent+9]=this.dw_3
this.Control[iCurrent+10]=this.dw_5
this.Control[iCurrent+11]=this.dw_6
end on

on w_mant_instructivo_emb.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_2)
destroy(this.cb_3)
destroy(this.cb_4)
destroy(this.dw_enca)
destroy(this.dw_pallets)
destroy(this.cb_agrega)
destroy(this.cb_elimina)
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.dw_5)
destroy(this.dw_6)
end on

event resize;call super::resize;dw_enca.x		= 37 + Round((This.WorkSpaceWidth()  - dw_enca.width) / 2, 0)
dw_2.x			= (dw_enca.x + (dw_enca.width / 2)) - (dw_2.width / 2)
dw_2.y			= dw_enca.y + dw_enca.Height + 20

dw_1.x			= 37 
dw_1.y			= dw_enca.y + dw_enca.Height + dw_2.Height + 20
dw_1.Width		= This.WorkSpaceWidth() - 400
dw_1.Height		=  (dw_pallets.y - dw_1.y ) - 30

dw_Pallets.x			=	dw_1.x
dw_Pallets.Height	= This.WorkSpaceHeight() - dw_Pallets.y - 41

cb_Agrega.x	=dw_Pallets.x + dw_Pallets.Width + 20	
cb_Elimina.x	=cb_Agrega.x	

dw_4.x			= cb_Agrega.x	+ cb_Agrega.Width + 20	
dw_4.y			= dw_pallets.y
dw_4.Width		= dw_1.Width - dw_4.x + 40
dw_4.Height		= This.WorkSpaceHeight() - dw_4.y - 41

cb_4.x	= pb_ins_det.x
cb_3.x	= cb_4.x
cb_2.x	= cb_3.x

cb_4.y	= pb_ins_det.y - cb_4.height
cb_3.y	= cb_4.y - cb_4.height
cb_2.y	= cb_3.y - cb_4.height
end event

event ue_imprimir;//
Long		fila
integer li_planta, li_cliente, li_varirotula
String  ls_cliente, ls_planta, ls_numero, ls_hora, ls_fecha, ls_responsable, ls_embarque
datawindowchild dwc_planta

SetPointer(HourGlass!)

li_varirotula = MessageBox("Impresión",'¿Imprime Variedad Rotulada?',Exclamation!,YEsNo!, 2)

IF li_varirotula = 1 THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

IF ii_tipo = 1 THEN
	istr_info.titulo	= "PALLETS PARA DESPACHO"
	istr_info.copias	= 1

	OpenWithParm(vinf,istr_info)
	
	vinf.dw_1.DataObject = "dw_info_despaselec2"
ELSE
	istr_info.titulo	= "PUCHOS A REPALLETIZAR"
	istr_info.copias	= 1

	OpenWithParm(vinf,istr_info)
	
	vinf.dw_1.DataObject = "dw_info_despaselec2_repa"
END IF

li_planta = dw_enca.Object.plde_codigo[1]
li_cliente = dw_enca.Object.clie_codigo[1]
ii_dese_numero = dw_enca.Object.dese_numero[1]

Busca_Observaciones(li_cliente,li_planta,ii_dese_numero)
//wf_actualiza_db(FALSE)

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(li_cliente, li_planta, ii_dese_numero,ii_tipo,li_varirotula)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	ls_planta 		= nombre_planta(li_planta)
	ls_cliente 		= nombre_cliente(li_cliente)	
	ls_numero		= string(ii_dese_numero)
	ls_hora			= string(dw_enca.Object.dese_horase[1])
	ls_responsable = dw_enca.Object.dese_respon[1]
	ls_embarque		= dw_enca.Object.embq_codigo[1]
	ls_fecha			= string(dw_enca.Object.dese_fecsel[1])
	
	IF not isnull(dw_enca.Object.embq_codigo[1]) OR dw_enca.Object.embq_codigo[1] <> '' THEN
		datos_embarque(dw_enca.Object.embq_codigo[1])
			
		vinf.dw_1.Modify("t_embarque.text = '" + ls_embarque + "'")
		vinf.dw_1.Modify("t_nave.text = '" + is_nave + "'")
		vinf.dw_1.Modify("t_origen.text = '" + is_puerto + "'")
		vinf.dw_1.Modify("t_puerto.text = '" + is_destino + "'")
		vinf.dw_1.Modify("t_recibidor.text = '" + is_recibidor + "'")
		vinf.dw_1.Modify("t_fzarpe.text = '" + is_fzarpe + "'")
	END IF
	
	IF li_varirotula = 1 THEN
		vinf.dw_1.Modify("t_varirotula.text = '" + 'Rotulada' + "'")
	ELSE
		vinf.dw_1.Modify("t_varirotula.text = '" + 'Real' + "'")
	END IF
			
	vinf.dw_1.Modify("t_fecha.text = '" + ls_fecha + "'")
	vinf.dw_1.Modify("t_cliente.text = '" + ls_cliente + "'")
	vinf.dw_1.Modify("t_planta.text = '" + ls_planta + "'")
	vinf.dw_1.Modify("t_seleccion.text = '" + ls_numero + "'")
	vinf.dw_1.Modify("t_hora.text = '" + ls_hora + "'")
	vinf.dw_1.Modify("t_responsable.text = '" + ls_responsable + "'")
	vinf.dw_1.Modify("t_obs1.text = '" + is_obs1 + "'")
	vinf.dw_1.Modify("t_obs2.text = '" + is_obs2 + "'")
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
//
end event

event ue_seleccion;call super::ue_seleccion;string ls_embq
istr_busq.argum[1]	=	String(dw_enca.Object.clie_codigo[1]) 
istr_busq.argum[2]	=	string(dw_enca.Object.plde_codigo[1])

istr_busq.argum[10]	=  String(ii_tipo)

OpenWithParm(w_busc_despaselecenca, istr_busq)
istr_busq = Message.PowerObjectParm

If istr_busq.argum[5] <> "" Then
	ii_dese_numero = Long(istr_busq.argum[5])
	
	dw_enca.Object.clie_codigo[1]		=	Long(istr_busq.argum[1])
	dw_enca.Object.plde_codigo[1]	=	Long(istr_busq.argum[2])
	
	
	If ExisteDespa(ii_dese_numero, Long(istr_busq.argum[1]), Long(istr_busq.argum[2])) Then
		This.TriggerEvent("ue_recuperadatos")
	Else
		This.TriggerEvent("ue_recuperadatos")
	End If
Else
	pb_buscar.SetFocus()
End If
end event

event open;integer li_row

y				=	0
This.Height	=	2520
im_menu		=	m_principal

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
//dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
								
dw_enca.SetTransObject(SQLCA)
dw_1.SetTransObject(SQLCA)
dw_pallets.SetTransObject(SQLCA)
dw_5.SetTransObject(SQLCA)

li_row = dw_enca.InsertRow(0)

dw_enca.GetChild("clie_codigo", idwc_cliente)
dw_enca.GetChild("plde_codigo", idwc_planta)

idwc_cliente.SetTransObject(sqlca)
idwc_planta.SetTransObject(sqlca)

idwc_cliente.Retrieve()
idwc_planta.Retrieve()

istr_busq.argum[1] = String(gi_CodExport)
istr_busq.argum[3] = String(gi_CodPlanta)
istr_busq.argum[5] = '0'

dw_enca.Object.clie_codigo[li_row] = gi_CodExport
dw_enca.Object.plde_codigo[li_row] = gi_CodPlanta
dw_enca.Object.dese_fecsel[li_row] = Date(String(Today(),"dd-mm-yyyy"))

ii_tipo = Integer(Message.StringParm)

IF ii_tipo = 2 THEN
	dw_enca.SetTabOrder("embq_codigo",0)
	dw_enca.Modify("embq_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_enca.Object.b_embq.Enabled = False
END IF	


end event

event ue_nuevo;String ls_null
Integer li_row

SetNull(ls_null)

dw_3.reset()
dw_4.Reset()
dw_enca.reset()
dw_pallets.reset()
dw_5.Reset()
dw_1.reset()
dw_2.reset()

dw_pallets.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_2.InsertRow(0)
li_row = dw_enca.InsertRow(0)

dw_enca.GetChild("clie_codigo", idwc_cliente)
dw_enca.GetChild("plde_codigo", idwc_planta)

idwc_cliente.SetTransObject(sqlca)
idwc_planta.SetTransObject(sqlca)
dw_5.SetTransObject(SQLCA)

idwc_cliente.Retrieve()
idwc_planta.Retrieve()

istr_busq.argum[1] = string(gi_CodExport)
istr_busq.argum[3] = string(gi_CodPlanta)

dw_enca.Object.clie_codigo[li_row] = gi_CodExport
dw_enca.Object.plde_codigo[li_row] = gi_CodPlanta
dw_enca.Object.dese_horase[li_row] = Time(Now())

IF ii_tipo = 1 THEN
	dw_enca.Object.dece_tipsel[li_row] = 1
ELSE
	dw_enca.Object.dece_tipsel[li_row] = 2
END IF

dw_enca.Object.dese_fecsel[li_row] = Date(String(Today(),"dd-mm-yyyy"))
dw_enca.Object.Embq_Codigo[1] = ls_null
dw_enca.SetColumn("Embq_Codigo")

dw_enca.SetTabOrder("dese_numero",10)
IF ii_tipo = 1 THEN
	dw_enca.SetTabOrder("embq_codigo",20)
	dw_enca.Object.b_embq.Enabled = True
END IF	
dw_enca.SetTabOrder("clie_codigo",30)
dw_enca.SetTabOrder("plde_codigo",40)
dw_enca.SetTabOrder("dese_fecsel",50)	
dw_enca.SetTabOrder("dese_respon",60)	

dw_enca.Modify("dese_numero.BackGround.Color = " + String(rgb(255,255,255)))
IF ii_tipo = 1 THEN
	dw_enca.Modify("embq_codigo.BackGround.Color = " + String(rgb(255,255,255)))	
	dw_enca.Object.b_embq.Enabled = True
END IF	
dw_enca.Modify("clie_codigo.BackGround.Color = " + String(rgb(255,255,255)))		
dw_enca.Modify("plde_codigo.BackGround.Color = " + String(rgb(255,255,255)))		
dw_enca.Modify("dese_fecsel.BackGround.Color = " + String(rgb(255,255,255)))		
dw_enca.Modify("dese_respon.BackGround.Color = " + String(rgb(255,255,255)))		
		
istr_busq.argum[5] =  String(buscanuevofolio(dw_enca.Object.plde_codigo[1]))

IF istr_busq.argum[5] = '0' THEN
	MessageBox("Advertencia","Falta Ingreso de Número de Correlativos en Mantenedor'")
	Return
END IF	

il_total = 0
pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False

dw_enca.SetFocus()
end event

event ue_recuperadatos;call super::ue_recuperadatos;integer 	li_fila = 1, li_cliente, li_planta, respuesta, li_ret, li_i, li_first, li_j, li_productor
integer 	li_plde_codigo, li_especie, li_variedad, li_etiqueta,li_clie_codigo, row, li_inspeccion,&
			li_destino,li_status, li_secuencia, li_rechazo
string 	ls_embalaje, ls_calibre, ls_nota
date 		fecemb_desde, fecemb_hasta
Long		ll_dese_numero

datawindowchild dwc_variedad

DO
	li_cliente 	= 	dw_enca.Object.Clie_codigo[1]
	li_planta		= 	dw_enca.Object.plde_codigo[1]
	dw_1.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(SQLCA)
	idwc_productor.Retrieve(li_cliente)
	idwc_productor.InsertRow(1)
		
	dw_enca.Object.embq_codigo[1] = ii_embqcod
	
	IF ii_tipo = 1 THEN li_fila = dw_2.retrieve(li_Cliente, ii_embqcod)
	
	li_fila = 1
	
	IF li_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF li_fila > 0 THEN
		
		il_total = dw_enca.Retrieve(li_cliente, ii_dese_numero, li_planta,ii_tipo)
		dw_enca.Object.dese_numero.Protect	= 1
		dw_enca.Object.clie_codigo.Protect 		= 1
		dw_enca.Object.plde_codigo.Protect 		= 1
		dw_enca.Object.dese_fecsel.Protect 		= 1
		dw_enca.Object.dese_respon.Protect 	= 1

		dw_enca.Object.dese_numero.Color	= RGB(255,255,255)
		dw_enca.Object.clie_codigo.Color 		= RGB(255,255,255)
		dw_enca.Object.plde_codigo.Color 	= RGB(255,255,255)
		dw_enca.Object.dese_fecsel.Color 	= RGB(255,255,255)
		dw_enca.Object.dese_respon.Color 	= RGB(255,255,255)
		
		dw_enca.Object.dese_numero.BackGround.Color	= 553648127
		dw_enca.Object.clie_codigo.BackGround.Color 		= 553648127
		dw_enca.Object.plde_codigo.BackGround.Color 	= 553648127
		dw_enca.Object.dese_fecsel.BackGround.Color 	= 553648127
		dw_enca.Object.dese_respon.BackGround.Color 	= 553648127
		
		li_fila = dw_1.Retrieve(li_cliente,dw_enca.Object.dese_numero[1],li_planta,ii_tipo)
		IF li_fila = -1 THEN
			respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
		ELSE
			//IF li_fila > 0 THEN
				FOR row = 1 to dw_1.RowCount()
					
					li_clie_codigo = dw_enca.Object.Clie_Codigo[1]
					li_especie		= -1
					li_variedad		= -1
					ls_embalaje		= '*'
					li_etiqueta		= -1
					ls_calibre		= 'TOD'
					li_plde_codigo = dw_enca.object.plde_codigo[dw_enca.GetRow()]
					li_status		= -1
					li_destino		= -1
					li_secuencia	= -1
					li_rechazo		= 0
					
					IF NOT isnull(dw_1.object.espe_codigo[row]) THEN li_especie		= dw_1.object.espe_codigo[row]
					IF NOT isnull(dw_1.object.vari_codigo[row]) THEN li_variedad	= dw_1.object.vari_codigo[row]
					IF NOT isnull(dw_1.object.emba_codigo[row]) THEN ls_embalaje	= dw_1.object.emba_codigo[row]
					IF NOT isnull(dw_1.object.etiq_codigo[row]) THEN li_etiqueta	= dw_1.object.etiq_codigo[row]
					IF NOT isnull(dw_1.object.paen_calibr[row]) THEN ls_calibre	   = dw_1.object.paen_calibr[row]
					IF NOT isnull(dw_1.object.prod_codigo[row]) THEN li_productor	= dw_1.object.prod_codigo[row]
					IF NOT isnull(dw_1.object.dest_codigo[row]) THEN li_destino		= dw_1.object.dest_codigo[row]
					IF NOT isnull(dw_1.object.dese_secdet[row]) THEN li_secuencia	= dw_1.object.dese_secdet[row]
					IF NOT isnull(dw_1.object.stat_codigo[row]) THEN li_status		= dw_1.object.stat_codigo[row]
					IF NOT isnull(dw_1.object.inpe_numero[row]) OR dw_1.object.inpe_numero[row] <> 0 THEN li_inspeccion 	= dw_1.object.inpe_numero[row]
					IF NOT isnull(dw_1.object.dese_rechaz[row]) THEN li_rechazo		= dw_1.object.dese_rechaz[row]
					IF NOT isnull(dw_1.object.pafr_barra2[row]) THEN ls_nota 		= dw_1.object.pafr_barra2[row]
					
					wf_activa_filtros_dw1("especie", "","")
					IF li_especie <> -1 then wf_activa_filtros_dw1("variedad", string(li_especie),"")
					wf_activa_filtros_dw1("variedad", string(li_especie),"")
					wf_activa_filtros_dw1("embalaje", string(li_cliente),"")
					wf_activa_filtros_dw1("etiqueta", "","")
					wf_activa_filtros_dw1("calibre", "","")
					wf_activa_filtros_dw1("productor", "","")
					wf_activa_filtros_dw1("destino", "","")
					wf_activa_filtros_dw1("secuencia", string(li_especie),string(li_destino))
					wf_activa_filtros_dw1("status", "","")
					
					IF ISDATE(String(dw_1.object.dese_fecini[row], 'dd-mm-yyyy')) THEN 
						fecemb_desde						= dw_1.object.dese_fecini[row]
					ELSE
						fecemb_desde						= Date('01-01-1900')
						dw_1.object.dese_fecini[row] 	= fecemb_desde
					END IF
					
					IF ISDATE(String(dw_1.object.dese_fecter[row], 'dd-mm-yyyy')) THEN 
						fecemb_hasta						= Date(dw_1.object.dese_fecter[row])
					ELSE
						fecemb_hasta						= Date(string(Today(), 'dd-mm-yyyy'))
						dw_1.object.dese_fecter[row] 	= fecemb_hasta
					END IF
					
					dw_4.SetTransObject(SQLCA)
					
					ll_dese_numero	=	dw_enca.Object.dese_numero[1]
					
					li_ret = dw_4.retrieve(li_clie_codigo, li_plde_codigo, li_especie, li_variedad, ls_embalaje,&
								li_etiqueta, fecemb_desde, fecemb_hasta,ls_calibre,li_productor,ii_tipo,ll_dese_numero,&
								li_inspeccion,li_destino,li_status,li_secuencia,li_rechazo,ls_nota)
					
					dw_1.Object.dese_secuen[row] = row
					dw_1.SetRow(row)
					
					dw_pallets.Retrieve(li_clie_codigo, li_plde_codigo, ii_dese_numero,ii_tipo)
					
					IF li_ret > 0 THEN
						
						FOR li_i = 1 TO dw_4.RowCount()
							dw_4.object.dese_secuen[li_i] = dw_1.object.dese_secuen[row]
						NEXT
						
						pb_grabar.Enabled = True
						
//						dw_4.SetSort("#1 asc")
//						dw_4.Sort()
					ELSE
						Messagebox("Atención","No existe Pallets que cumplan con los parametros de el filtro " + String(row), Exclamation!)
						IF dw_pallets.RowCount() > 0 THEN
							pb_imprimir.Enabled = True
						END IF	
					END IF
				NEXT
			//END IF	
			
		END IF
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(THIS)
end event

event ue_antesguardar;call super::ue_antesguardar;Integer 	li_clie_codigo, li_especie, li_variedad, li_etiqueta, li_plde_codigo, row, li_productor,&
			li_status, li_destino, li_secuencia, li_rechazo
String 	ls_embalaje, ls_calibre, ls_nota
Date 		fecemb_desde, fecemb_hasta
Long		ll_Secuen=0,ll_mayor

li_clie_codigo = dw_enca.Object.Clie_Codigo[1]
li_plde_codigo = dw_enca.object.plde_codigo[1]//[dw_enca.GetRow()]

li_especie		= -1
li_variedad		= -1
ls_embalaje		= '*'
li_etiqueta		= -1
ls_calibre		= 'TOD'
li_productor	= -1
li_status		= -1
li_destino		= -1
li_secuencia	= -1
li_rechazo		= 0
ls_nota			= '-1'

//FOR ROW = 1 TO dw_1.RowCount()
//	ll_Secuen	=	dw_1.Object.dese_secuen[ROW]
//	IF ll_Secuen > ll_mayor THEN
//	   ll_mayor = ll_Secuen
//	END IF
//NEXT

FOR ROW = 1 TO dw_1.RowCount()
	
	IF isnull(dw_1.object.espe_codigo[row]) THEN dw_1.object.espe_codigo[row] = li_especie
	IF isnull(dw_1.object.vari_codigo[row]) THEN dw_1.object.vari_codigo[row] = li_variedad
	IF isnull(dw_1.object.emba_codigo[row]) THEN dw_1.object.emba_codigo[row] = ls_embalaje
	IF isnull(dw_1.object.etiq_codigo[row]) THEN dw_1.object.etiq_codigo[row] = li_etiqueta
	IF isnull(dw_1.object.paen_calibr[row]) THEN dw_1.object.paen_calibr[row] = ls_calibre
	IF isnull(dw_1.object.prod_codigo[row]) THEN dw_1.object.prod_codigo[row] = li_productor
	IF isnull(dw_1.object.dest_codigo[row]) THEN dw_1.object.dest_codigo[row] = li_destino
	IF isnull(dw_1.object.stat_codigo[row]) THEN dw_1.object.stat_codigo[row] = li_status
	IF isnull(dw_1.object.dese_secdet[row]) THEN dw_1.object.dese_secdet[row] = li_secuencia
	IF isnull(dw_1.object.dese_rechaz[row]) THEN dw_1.object.dese_rechaz[row] = li_rechazo
	IF isnull(dw_1.object.pafr_barra2[row]) THEN dw_1.object.pafr_barra2[row] = ls_nota
	
	dw_1.object.clie_codigo[row] = li_clie_codigo
	dw_1.object.plde_codigo[row] = li_plde_codigo
	dw_1.object.dese_numero[row] = Long(istr_busq.argum[5])

//	IF IsNull(dw_1.Object.dese_secuen[row]) THEN
//		ll_mayor = ll_mayor + 1
//		dw_1.Object.dese_secuen[row]	=	ll_mayor
//	END IF
	
	IF ISDATE(String(dw_1.object.dese_fecini[row], 'dd-mm-yyyy')) THEN 
		fecemb_desde						= dw_1.object.dese_fecini[row]
	ELSE
		fecemb_desde						= Date('01-01-1900')
		dw_1.object.dese_fecini[row] 	= fecemb_desde
	END IF
	
	IF ISDATE(String(dw_1.object.dese_fecter[row], 'dd-mm-yyyy')) THEN 
		fecemb_hasta						= Date(dw_1.object.dese_fecter[row])
	ELSE
		fecemb_hasta						= Date(string(Today(), 'dd-mm-yyyy'))
		dw_1.object.dese_fecter[row] 	= fecemb_hasta
	END IF
NEXT

dw_enca.AcceptText()
dw_1.AcceptText()
dw_pallets.AcceptText()
end event

event closequery;IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			Message.ReturnValue = 1 
		CASE 0
			IF dw_1.RowCount() > 0 THEN
//				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
//					CASE 1
//						Message.DoubleParm = 0
//						This.triggerevent("ue_guardar")
//						IF message.doubleparm = -1 THEN Message.ReturnValue = 1
//						RETURN
//					CASE 3
//						Message.ReturnValue = 1
//						RETURN
//				END CHOOSE
			END IF
	END CHOOSE
END IF
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_mant_instructivo_emb
integer x = 32
integer y = 536
integer width = 3205
integer height = 580
integer taborder = 90
boolean titlebar = false
string dataobject = "dw_mant_despaselecdeta"
boolean ib_allow_updates = false
end type

event dw_1::itemchanged;call super::itemchanged;integer li_ret, li_null
String ls_Columna
datawindowchild ldwc_variedad, ldwc_embalajesprod

SetNull(li_null)

ls_columna = dwo.name
CHOOSE CASE ls_columna
	CASE "espe_codigo" 
		wf_activa_filtros_dw1("especie", data,"")
		wf_activa_filtros_dw1("variedad", data,"")
		wf_activa_filtros_dw1("embalaje", data,"")
		wf_activa_filtros_dw1("destino", String(data),"")
	
CASE "dest_codigo" 
	
		wf_activa_filtros_dw1("destino", String(dw_1.Object.espe_codigo[row]),data)	
		dw_1.SetItem(Row,'dese_secdet',Integer(li_null))
		
END CHOOSE
end event

event dw_1::buttonclicked;call super::buttonclicked;Integer 	li_plde_codigo, li_especie, li_variedad, li_etiqueta,li_clie_codigo, li_destino, li_inspeccion,&
			li_status,li_ret, li_i, li_first, li_j, li_productor, li_secuencia, li_rechazo
String 	ls_embalaje, ls_calibre, ls_nota
Date 		fecemb_desde, fecemb_hasta
Long		ll_cont,ll_dese_numero

datawindowchild dwc_variedad

dw_6.SetTransObject(sqlca)

li_clie_codigo 	= dw_enca.Object.Clie_Codigo[1]
li_especie		= -1
li_variedad		= -1
ls_embalaje		= '*'
li_etiqueta		= -1
ls_calibre		= 'TOD'
li_productor		= -1
li_destino		= -1
li_inspeccion  	= -1
li_status  		= -1
li_secuencia 	= -1
li_rechazo		= 0
ls_nota			= '-1'
		
Choose Case dwo.name	
	Case "b_pallets" 
		li_plde_codigo = dw_enca.object.plde_codigo[dw_enca.GetRow()]
		
		If NOT isnull(THIS.object.espe_codigo[row]) Then li_especie		= THIS.object.espe_codigo[row]
		If NOT isnull(THIS.object.vari_codigo[row]) Then li_variedad	= THIS.object.vari_codigo[row]
		If NOT isnull(THIS.object.emba_codigo[row]) Then ls_embalaje	= THIS.object.emba_codigo[row]
		If NOT isnull(THIS.object.etiq_codigo[row]) Then li_etiqueta	= THIS.object.etiq_codigo[row]
		If NOT isnull(THIS.object.paen_calibr[row]) Then ls_calibre 	= THIS.object.paen_calibr[row]
		If NOT isnull(THIS.object.prod_codigo[row]) Then li_productor 	= THIS.object.prod_codigo[row]
		If NOT isnull(THIS.object.dest_codigo[row]) Then li_destino 	= THIS.object.dest_codigo[row]
		If NOT isnull(THIS.object.dese_secdet[row]) Then li_secuencia 	= THIS.object.dese_secdet[row]
		If NOT isnull(THIS.object.inpe_numero[row]) Then li_inspeccion = THIS.object.inpe_numero[row]
		If NOT isnull(THIS.object.stat_codigo[row]) Then li_status 		= THIS.object.stat_codigo[row]
		If NOT isnull(THIS.object.dese_rechaz[row]) Then li_rechazo 	= THIS.object.dese_rechaz[row]	
		If NOT isnull(THIS.object.pafr_barra2[row]) Then ls_nota 		= THIS.object.pafr_barra2[row]
		
		If ISDATE(String(THIS.object.dese_fecini[row], 'dd-mm-yyyy')) Then 
			fecemb_desde						= THIS.object.dese_fecini[row]
		Else
			fecemb_desde						= Date('01-01-1900')
			THIS.object.dese_fecini[row] 	= fecemb_desde
		End If
		
		If ISDATE(String(THIS.object.dese_fecter[row], 'dd-mm-yyyy')) Then 
			fecemb_hasta						= Date(THIS.object.dese_fecter[row])
		Else
			fecemb_hasta						= Date(string(Today(), 'dd-mm-yyyy'))
			THIS.object.dese_fecter[row] 	= fecemb_hasta
		End If
		
		ll_cont = dw_6.Retrieve(li_clie_codigo, li_plde_codigo, dw_1.object.dese_numero[row],ii_tipo)
		
		If ll_cont > 0 Then
			dw_pallets.Retrieve(li_clie_codigo, li_plde_codigo, dw_1.object.dese_numero[row],ii_tipo)
		End If
		
		dw_4.SetTransObject(SQLCA)
		
		ll_dese_numero = dw_enca.Object.dese_numero[1]
		
		li_ret = dw_4.retrieve(li_clie_codigo, li_plde_codigo, li_especie, li_variedad, ls_embalaje, li_etiqueta,&
						fecemb_desde, fecemb_hasta,ls_calibre,li_productor,ii_tipo,ll_dese_numero,li_inspeccion,&
						li_destino,li_status,li_secuencia,li_rechazo,ls_nota)
		
		dw_1.Object.dese_secuen[row] = row
		
		If li_ret > 0 Then
			FOR li_i = 1 TO dw_4.RowCount()
				dw_4.object.dese_secuen[li_i] = dw_1.object.dese_secuen[row]
			NEXT		
		Else
			Messagebox("Atención","No existe Pallets que cumplan con los parametros de busqueda", Exclamation!)
		End If
		
	Case "b_secuencia"
		istr_busq.argum[11] = string(dw_1.object.espe_codigo[row])
		istr_busq.argum[12] = string(dw_1.object.dest_codigo[row])
		
		If dw_1.object.dest_codigo[row] <> -1 Then
			OpenWithParm(w_busc_secuencia_destino, istr_busq)
			
			istr_busq = Message.PowerObjectParm
			
			If istr_busq.argum[10] <> "" Then
				dw_1.Object.dese_secdet[row] = integer(istr_busq.argum[13])
			End If	
		End If	
		
End Choose













end event

event dw_1::rowfocuschanging;call super::rowfocuschanging;//IF dw_pallets.DeletedCount() > 0 OR dw_pallets.ModifiedCount() > 0 THEN
//  IF Messagebox("Advertencia", "Ha modificado pallets del filtro Activo y estos no han sido grabados~r ¿Desea guardar los cambios?", Question!, YesNo!, 2) = 2 Then
//		//this.SetRow(dw_pallets.Object.dese_secuen[1])
//		RETURN 0
//	ELSE
//		w_mant_instructivo_emb.TriggerEvent("ue_guardar")
//		RETURN 0
//	END IF
//ELSE 
//   RETURN 0
//END IF 
end event

event dw_1::getfocus;//this.Title = "Pallets Disponibles"
dw_pallets.Title = "Pallets Selec."
dw_4.Title = "Pallets a Seleccionar."
end event

event dw_1::clicked;call super::clicked;Integer 	li_plde_codigo, li_especie, li_variedad, li_etiqueta,li_clie_codigo, li_destino, li_inspeccion,&
			li_status,li_ret, li_i, li_first, li_j, li_productor, li_secuencia,li_rechazo
String ls_embalaje, ls_calibre, ls_nota
Date fecemb_desde, fecemb_hasta
Long	ll_cont,ll_dese_numero
datawindowchild dwc_variedad

IF row > 0 THEN
	li_clie_codigo = dw_enca.Object.Clie_Codigo[1]
	li_especie		= -1
	li_variedad		= -1
	ls_embalaje		= '*'
	li_etiqueta		= -1
	ls_calibre		= 'TOD'
	li_productor	= -1
	li_destino		= -1
	li_inspeccion  = -1
	li_status  		= -1
	li_secuencia	= -1
	li_rechazo		= 0
	ls_nota			= '-1'
	
	IF dw_1.RowCount() > 1 THEN		
		
		li_plde_codigo = dw_enca.object.plde_codigo[dw_enca.GetRow()]
		
		IF NOT isnull(THIS.object.espe_codigo[row]) THEN li_especie		= THIS.object.espe_codigo[row]
		IF NOT isnull(THIS.object.vari_codigo[row]) THEN li_variedad	= THIS.object.vari_codigo[row]
		IF NOT isnull(THIS.object.emba_codigo[row]) THEN ls_embalaje	= THIS.object.emba_codigo[row]
		IF NOT isnull(THIS.object.etiq_codigo[row]) THEN li_etiqueta	= THIS.object.etiq_codigo[row]
		IF NOT isnull(THIS.object.paen_calibr[row]) THEN ls_calibre 	= THIS.object.paen_calibr[row]
		IF NOT isnull(THIS.object.prod_codigo[row]) THEN li_productor 	= THIS.object.prod_codigo[row]
		IF NOT isnull(THIS.object.dest_codigo[row]) THEN li_destino 	= THIS.object.dest_codigo[row]
		IF NOT isnull(THIS.object.dese_secdet[row]) THEN li_secuencia 	= THIS.object.dese_secdet[row]
		IF NOT isnull(THIS.object.inpe_numero[row]) THEN li_inspeccion = THIS.object.inpe_numero[row]
		IF NOT isnull(THIS.object.stat_codigo[row]) THEN li_status 		= THIS.object.stat_codigo[row]
		IF NOT isnull(THIS.object.dese_rechaz[row]) THEN li_rechazo		= THIS.object.dese_rechaz[row]
		IF NOT isnull(THIS.object.pafr_barra2[row]) THEN ls_nota 		= THIS.object.pafr_barra2[row]
		
		IF ISDATE(String(THIS.object.dese_fecini[row], 'dd-mm-yyyy')) THEN 
			fecemb_desde						= THIS.object.dese_fecini[row]
		ELSE
			fecemb_desde						= Date('01-01-1900')
			THIS.object.dese_fecini[row] 	= fecemb_desde
		END IF
		
		IF ISDATE(String(THIS.object.dese_fecter[row], 'dd-mm-yyyy')) THEN 
			fecemb_hasta						= Date(THIS.object.dese_fecter[row])
		ELSE
			fecemb_hasta						= Date(string(Today(), 'dd-mm-yyyy'))
			THIS.object.dese_fecter[row] 	= fecemb_hasta
		END IF
			
		dw_4.SetTransObject(SQLCA)
		
		ll_dese_numero = dw_enca.Object.dese_numero[1]
		
		li_ret = dw_4.retrieve(li_clie_codigo, li_plde_codigo, li_especie, li_variedad, ls_embalaje, li_etiqueta,&
						fecemb_desde, fecemb_hasta,ls_calibre,li_productor,ii_tipo,ll_dese_numero,li_inspeccion,&
						li_destino,li_status,li_secuencia,li_rechazo,ls_nota)
		
	END IF	
END IF	
	

end event

type dw_2 from w_mant_encab_deta`dw_2 within w_mant_instructivo_emb
integer x = 32
integer y = 408
integer width = 3205
integer height = 128
integer taborder = 0
string dataobject = "dw_mues_embarque_chico"
boolean livescroll = true
borderstyle borderstyle = styleraised!
end type

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_mant_instructivo_emb
integer x = 3351
integer taborder = 120
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_mant_instructivo_emb
integer x = 3351
integer taborder = 130
end type

event pb_eliminar::clicked;String ls_null
Integer li_row
Long	ll_dese_numero


SetNull(ls_null)

IF MessageBox("Borrar registro(s)","¿Desea Borrar la información?",Exclamation!, OKCancel!, 2) = 2 THEN
	Return 0
END IF

ll_dese_numero = dw_enca.Object.dese_numero[1]
DECLARE limpia PROCEDURE FOR dbo.sp_limpia_tabla_despaselec
	@dese_numero =:ll_dese_numero
USING sqlca;

EXECUTE limpia;
IF sqlca.SQLErrText <> "" THEN
	Messagebox("Error", "Se ha generado un error al ejecutar ~r" + sqlca.SQLErrText)
	CLOSE limpia;
	RETURN 
END IF
CLOSE limpia;

dw_1.Reset()
dw_3.reset()
dw_4.Reset()
dw_enca.reset()
dw_pallets.reset()

dw_pallets.SetTransObject(sqlca)

li_row = dw_enca.InsertRow(0)

dw_enca.GetChild("clie_codigo", idwc_cliente)
dw_enca.GetChild("plde_codigo", idwc_planta)

idwc_cliente.SetTransObject(sqlca)
idwc_planta.SetTransObject(sqlca)

idwc_cliente.Retrieve()
idwc_planta.Retrieve()

istr_busq.argum[1] = string(gi_CodExport)
istr_busq.argum[3] = string(gi_CodPlanta)

dw_enca.Object.clie_codigo[li_row] = gi_CodExport
dw_enca.Object.plde_codigo[li_row] = gi_CodPlanta

istr_busq.argum[5] =  String(buscanuevofolio(dw_enca.Object.plde_codigo[1]))

dw_enca.Object.dese_fecsel[li_row] = Date(String(Today(),"dd-mm-yyyy"))
dw_enca.Object.Embq_Codigo[1] = ls_null
dw_enca.SetColumn("Embq_Codigo")

dw_enca.SetTabOrder("dese_numero",10)
dw_enca.SetTabOrder("embq_codigo",20)
dw_enca.SetTabOrder("clie_codigo",30)
dw_enca.SetTabOrder("plde_codigo",40)
dw_enca.SetTabOrder("dese_fecsel",50)		

dw_enca.Modify("dese_numero.BackGround.Color = " + String(rgb(255,255,255)))
dw_enca.Modify("embq_codigo.BackGround.Color = " + String(rgb(255,255,255)))	
dw_enca.Modify("clie_codigo.BackGround.Color = " + String(rgb(255,255,255)))		
dw_enca.Modify("plde_codigo.BackGround.Color = " + String(rgb(255,255,255)))		
dw_enca.Modify("dese_fecsel.BackGround.Color = " + String(rgb(255,255,255)))		
		

dw_enca.SetFocus()
end event

type pb_grabar from w_mant_encab_deta`pb_grabar within w_mant_instructivo_emb
integer x = 3351
integer y = 680
integer taborder = 140
boolean enabled = true
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_mant_instructivo_emb
integer x = 3351
integer taborder = 150
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_mant_instructivo_emb
integer x = 3351
integer taborder = 160
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_mant_instructivo_emb
integer x = 3355
integer y = 1276
integer taborder = 170
end type

event pb_ins_det::clicked;call super::clicked;Integer li_fila
DataWindowChild ldwc_embalajes

wf_activa_filtros_dw1("especie", "","")
wf_activa_filtros_dw1("variedad", string(gi_CodEspecie),"")
wf_activa_filtros_dw1("embalaje", string(gi_CodEspecie),"")
wf_activa_filtros_dw1("etiqueta", "","")
wf_activa_filtros_dw1("calibre", "","")
wf_activa_filtros_dw1("productor", "","")
wf_activa_filtros_dw1("destino", "","")
wf_activa_filtros_dw1("secuencia", string(gi_CodEspecie),"-1")
wf_activa_filtros_dw1("status", "","")

li_fila = dw_1.insertRow(0)

dw_1.Object.espe_codigo[li_fila] = gi_CodEspecie
dw_1.GetChild("emba_codigo", ldwc_embalajes)
dw_1.Object.dese_numero[li_fila] = dw_enca.Object.dese_numero[1]
IF ii_tipo = 1 THEN
	dw_1.Object.dece_tipsel[li_fila] = 1
	dw_1.Object.dese_secuen[li_fila] = li_fila
	dw_enca.Object.dece_tipsel[1] = 1
ELSE
	dw_1.Object.dece_tipsel[li_fila] = 2
	dw_enca.Object.dece_tipsel[1] = 2
	dw_1.Object.dese_secuen[li_fila] = li_fila	
END IF	
end event

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_mant_instructivo_emb
integer x = 3351
integer y = 1448
integer taborder = 180
end type

event pb_eli_det::clicked;call super::clicked;if messagebox("Confirmacion", "¿Esta seguro de borrar la informacion del filtro seleccionado?", question!, YesNo!) = 1 then
	dw_1.DeleteRow(dw_1.GetRow())
end if
end event

type pb_buscar from w_mant_encab_deta`pb_buscar within w_mant_instructivo_emb
integer x = 3351
integer taborder = 110
end type

event pb_buscar::clicked;pb_nuevo.PostEvent(Clicked!)

Parent.TriggerEvent("ue_seleccion")
end event

type cb_2 from uo_cbstandard within w_mant_instructivo_emb
string tag = "Ordena detalle"
integer x = 3351
integer y = 1792
integer width = 302
integer height = 92
integer taborder = 190
boolean bringtotop = true
fontcharset fontcharset = ansi!
string text = "Ordernar"
end type

event clicked;call super::clicked;String	ls_Retorno

str_parms	lstr_Parm

lstr_Parm.String_arg[1]	=	"Pallet:pallet," + &
									"fecha Embalaje:fecha_embalaje," + &
									"Productor:productor," + &
									"Cama:cama," + &
									"Calle:calle," + &
									"Base:base," + &
									"Posicion:posicion," + &
									"Cajas:cajas," + &
									"Inspeccion:inspeccion," + &
									"Condicion:condicion," + &
									"Estado:estado"
lstr_Parm.dw_arg			=	dw_4

OpenWithParm(w_columna_orden, lstr_Parm)

ls_Retorno	=	Message.StringParm

RETURN
end event

type cb_3 from uo_cbstandard within w_mant_instructivo_emb
string tag = "Selecciona Todos"
integer x = 3351
integer y = 1884
integer width = 302
integer height = 92
integer taborder = 70
boolean bringtotop = true
fontcharset fontcharset = ansi!
string text = "Todos"
end type

event clicked;call super::clicked;dw_4.SelectRow(0, TRUE)
end event

type cb_4 from uo_cbstandard within w_mant_instructivo_emb
string tag = "Desmarca Detalle"
integer x = 3351
integer y = 1976
integer width = 302
integer height = 92
integer taborder = 80
boolean bringtotop = true
fontcharset fontcharset = ansi!
string text = "Ninguno"
end type

event clicked;call super::clicked;dw_4.SelectRow(0, FALSE)
end event

type dw_enca from uo_dw within w_mant_instructivo_emb
integer x = 133
integer width = 2999
integer height = 392
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mant_despaselecenca"
boolean maxbox = true
boolean vscrollbar = false
boolean border = false
end type

event buttonclicked;call super::buttonclicked;long ll_clie_codigo, ll_filas
String ls_embq_codigo
string ls_boton, ls_nula

ls_boton = dwo.name
SetNull(ls_nula)

CHOOSE CASE ls_boton
	CASE "b_embq"
		istr_busq.argum[1] = string(dw_enca.object.clie_codigo[row])
		
		OpenWithParm(w_busc_embarque, istr_busq)
		
		istr_busq = Message.PowerObjectParm
		
		dw_2.SetTransObject(SQLCA)
		ll_filas = dw_2.retrieve(long(istr_busq.argum[1]), istr_busq.argum[2])
		
		IF ll_filas = 0 THEN
			Messagebox("Atencion", "No Existen datos para el Codigo de Embarque", Exclamation!)
			This.SetItem(1, "embq_codigo", Integer(ls_Nula))
			RETURN 1
		ELSE
			
			IF dw_4.RowCount() = 0 THEN
				istr_busq.argum[5] =  String(buscanuevofolio(dw_enca.Object.plde_codigo[1]))
				IF istr_busq.argum[5] = '0' THEN
					MessageBox("Advertencia","Falta Ingreso de Número de Correlativos en Mantenedor'")
					Return 1
				END IF 
				
				dw_enca.object.dese_numero[row] = Long(istr_busq.argum[5])
				This.Object.embq_codigo[row] = istr_busq.argum[2]
				IF Isnull(This.Object.dese_respon[Row]) OR This.Object.dese_respon[Row] = '' THEN
					pb_ins_det.Enabled = False
					pb_eli_det.Enabled = False
					//Messagebox("Atencion", "Falta el ingreso del responsable", Exclamation!)
					//This.SetItem(1, "dese_respon", ls_Nula)
					//RETURN 1
				ELSE
					pb_ins_det.Enabled = TRUE
					pb_eli_det.Enabled = TRUE
					//pb_grabar.Enabled = TRUE
					
				END IF	
			END IF	
		END IF
		This.Object.embq_codigo[row] = istr_busq.argum[2]
		
END CHOOSE
end event

event itemchanged;string ls_columna
long ll_clie_codigo
String ls_embq_codigo, ls_nula

ls_columna = dwo.name
SetNull(ls_nula)

CHOOSE CASE ls_columna
	CASE "embq_codigo"

		istr_busq.argum[1] = string(dw_enca.object.clie_codigo[1])
		istr_busq.argum[2] = DATA
		IF NOT buscaembarque() then
			This.SetItem(row, ls_Columna, ls_Nula)
			This.SetItem(row, "dese_numero", Long(ls_Nula))
			This.SetColumn(ls_columna)
			This.SetFocus()
			RETURN 1
		ELSE
			
			IF dw_4.RowCount() = 0 THEN
					
				IF istr_busq.argum[5] = '0' THEN
					istr_busq.argum[5] =  String(buscanuevofolio(dw_enca.Object.plde_codigo[1]))
					IF istr_busq.argum[5] = '0' THEN
						MessageBox("Advertencia","Falta Ingreso de Número de Correlativos en Mantenedor'")
						This.SetItem(row, ls_Columna, ls_Nula)
						This.SetColumn(ls_columna)
						This.SetFocus()
						Return 1
					END IF 
				END IF			
				IF Isnull(This.Object.dese_respon[Row]) OR This.Object.dese_respon[Row] = '' THEN
					pb_ins_det.Enabled = False
					pb_eli_det.Enabled = False
					//Messagebox("Atencion", "Falta el ingreso del responsable", Exclamation!)
					//This.SetItem(1, "dese_respon", ls_Nula)
					//RETURN 1
				ELSE
					pb_ins_det.Enabled = TRUE
					pb_eli_det.Enabled = TRUE
					pb_grabar.Enabled  = TRUE
				END IF
				dw_enca.object.dese_numero[row] = Long(istr_busq.argum[5])
			END IF
		END IF	
	CASE "clie_codigo"
		THIS.Reset()
		
		dw_enca.SetTransObject(SQLCA)
		dw_enca.InsertRow(0)
		
		THIS.Object.Clie_codigo[1] = integer(data)
		istr_busq.argum[1] = data
		
		dw_1.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(Integer(istr_busq.argum[1]))
		idwc_productor.InsertRow(1)
		idwc_productor.SetItem(1, "prod_nombre", 'TODOS')
		idwc_productor.SetItem(1, "prod_codigo", -1)
		
	CASE "dese_numero"
		ii_dese_numero = lONG(data)
		
		IF ExisteDespa(ii_dese_numero, dw_enca.Object.clie_codigo[1], dw_enca.Object.plde_codigo[1]) THEN
			
			Parent.TriggerEvent("ue_recuperadatos")
			istr_busq.argum[5] = String(dw_enca.Object.dese_numero[1])
			
		ELSE
			istr_busq.argum[5] =  String(buscanuevofolio(dw_enca.Object.plde_codigo[1]))
			This.SetItem(Row, ls_columna, Long(ls_Nula))
			
			String ls_null
			Integer li_row

			SetNull(ls_null)

			dw_3.reset()
			dw_4.Reset()
			dw_enca.reset()
			dw_pallets.reset()

			dw_pallets.SetTransObject(sqlca)

			dw_enca.InsertRow(0)

			dw_enca.GetChild("clie_codigo", idwc_cliente)
			dw_enca.GetChild("plde_codigo", idwc_planta)

			idwc_cliente.SetTransObject(sqlca)
			idwc_planta.SetTransObject(sqlca)

			idwc_cliente.Retrieve()
			idwc_planta.Retrieve()

			istr_busq.argum[1] = string(gi_CodExport)
			istr_busq.argum[3] = string(gi_CodPlanta)

			dw_enca.Object.clie_codigo[row] = gi_CodExport
			dw_enca.Object.plde_codigo[row] = gi_CodPlanta

			istr_busq.argum[5] =  String(buscanuevofolio(dw_enca.Object.plde_codigo[1]))

			dw_enca.Object.dese_fecsel[row] = Date(String(Today(),"dd-mm-yyyy"))
			dw_enca.Object.Embq_Codigo[1] = ls_null
			dw_enca.SetColumn("Embq_Codigo")
			dw_enca.SetFocus()
			
			dw_1.Reset()
			
			dw_2.Reset()
			dw_2.InsertRow(0)
			pb_grabar.Enabled 		= False
			pb_eli_det.Enabled		=	False
			pb_ins_det.Enabled		=	False
			pb_grabar.Enabled			=	False
			pb_eliminar.Enabled		=	False
			pb_imprimir.Enabled		=	False
			dw_2.Enabled				=	True
			RETURN 1
		END IF
	
	CASE "dese_respon"
		
		IF ii_tipo = 1 THEN
			IF Isnull(dw_enca.Object.embq_codigo[row]) OR dw_enca.Object.embq_codigo[row] = ''OR data = ''  THEN
				IF istr_busq.argum[5] = '0' THEN
					istr_busq.argum[5] =  String(buscanuevofolio(dw_enca.Object.plde_codigo[1]))
					IF istr_busq.argum[5] = '0' THEN
						MessageBox("Advertencia","Falta Ingreso de Número de Correlativos en Mantenedor'")
						This.SetItem(row, ls_Columna, ls_Nula)
						This.SetColumn(ls_columna)
						This.SetFocus()
						Return 1
					END IF 				
				END IF
				dw_enca.object.dese_numero[row] = Long(istr_busq.argum[5])
//				pb_ins_det.Enabled = False
//				pb_eli_det.Enabled = False
			ELSE
				pb_ins_det.Enabled = TRUE
				pb_eli_det.Enabled = TRUE
			END IF	
			pb_ins_det.Enabled = TRUE
			pb_eli_det.Enabled = TRUE
			pb_grabar.Enabled  = TRUE
		ELSE
			IF data <> '' THEN
				istr_busq.argum[5] =  String(buscanuevofolio(dw_enca.Object.plde_codigo[1]))
				dw_enca.object.dese_numero[row] = Long(istr_busq.argum[5])
				pb_ins_det.Enabled = TRUE
				pb_eli_det.Enabled = TRUE
			ELSE	
				pb_ins_det.Enabled = False
				pb_eli_det.Enabled = False
			END IF	
		END IF	
		
		
END CHOOSE
end event

event itemerror;Return 1
end event

type dw_pallets from uo_dw within w_mant_instructivo_emb
integer x = 14
integer y = 1120
integer width = 535
integer height = 944
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string title = "Pallets Selec."
string dataobject = "dw_mant_despaselecsele"
boolean hscrollbar = true
end type

event clicked;call super::clicked;Long	ll_fila, ll_pallet

IF row > 0 THEN
	FOR ll_fila = 1 TO dw_pallets.RowCount()
		
		ll_pallet = dw_pallets.Object.paen_numero[row]
		
		IF dw_pallets.Find("paen_numero = " + string(ll_pallet), ll_fila, ll_fila) > 0 THEN
			//THIS.IsSelected(ll_fila)
			THIS.SelectRow(ll_fila, Not(THIS.IsSelected(ll_fila)))
		END IF	
	NEXT	
ELSE
	THIS.SelectRow(row, Not(THIS.IsSelected(row)))
END IF	






end event

event getfocus;call super::getfocus;this.Title = "Pallets Selec."
dw_4.Title = ""
end event

event losefocus;call super::losefocus;this.Title = ""
end event

type cb_agrega from uo_cbstandard within w_mant_instructivo_emb
integer x = 571
integer y = 1452
integer width = 137
integer height = 60
integer taborder = 11
boolean bringtotop = true
string text = "<<"
end type

event clicked;call super::clicked;Long li_i, li_fila, li_pallet, li_secuen, ll_numero
Integer li_especie, li_variedad, li_productor, li_condicion, li_etiqueta, li_planta, li_cliente, li_destino,&
			li_inspeccion, li_status, li_rechazo
String	ls_calibr, ls_embalaje

FOR li_i =  1 to dw_4.RowCount()

	IF dw_4.IsSelected(li_i) THEN
		li_pallet = dw_4.Object.Pallet[li_i]
		li_secuen = dw_4.Object.secuencia[li_i]
		li_especie = dw_4.Object.espe_codigo[li_i]
		li_variedad = dw_4.Object.vari_codigo[li_i]
		ls_embalaje = dw_4.Object.emba_codigo[li_i]
		li_productor = dw_4.Object.productor[li_i]
		li_condicion = dw_4.Object.condicion[li_i]
		li_etiqueta = dw_4.Object.etiq_codigo[li_i]
		li_planta = dw_enca.Object.plde_codigo[1]
		ls_calibr = dw_4.Object.pafr_calibr[li_i]
		li_destino = dw_4.Object.dest_codigo[li_i]
		li_inspeccion = dw_4.Object.inpe_numero[li_i]		
		li_status	= dw_4.Object.stat_codigo[li_i]	
		
		IF dw_4.Object.existe[li_i] = 2 THEN
			MESSAGEBOX("CONTROL", "Pallet " + string(li_pallet)+ " Ya existe en Otra Selección Nº " + string(dw_4.Object.seleccion[li_i]) )
			
			FOR li_i =  1 to dw_4.RowCount()
				IF dw_4.IsSelected(li_i) THEN
					IF dw_4.Object.existe[li_i] = 2 THEN
						dw_4.SelectRow(li_i, Not(dw_4.IsSelected(li_i)))
					END IF	
				END IF
			NEXT	
			Return
		END IF	
		
		IF dw_pallets.Find("paen_numero = " + string(li_pallet) +&
								" AND espe_codigo = " + String(li_especie) + &
								" and pafr_calibr = '" + ls_calibr + "'" + &
								" AND vari_codigo = " + String(li_variedad) + &
								" AND emba_codigo = '" + ls_embalaje + "'" + &
								" AND prod_codigo = " + String(li_productor) + &
								" AND cond_codigo = " + String(li_condicion) + &
								" AND etiq_codigo = " + String(li_etiqueta) + &
								" AND plde_codigo = " + String(li_planta) + &
								" and pafr_secuen = " + string(li_secuen), 1, dw_pallets.RowCount()) < 1 THEN
								
			li_fila = dw_pallets.InsertRow(0)
			dw_pallets.Object.clie_codigo[li_fila] = dw_enca.Object.Clie_Codigo[1]
         dw_pallets.Object.plde_codigo[li_fila] = dw_enca.Object.Plde_Codigo[1]
			dw_pallets.Object.dese_numero[li_fila] = dw_enca.Object.dese_numero[1]
         dw_pallets.Object.dese_secuen[li_fila] = dw_4.Object.dese_secuen[li_i]
         dw_pallets.Object.paen_numero[li_fila] = li_pallet
			dw_pallets.Object.espe_codigo[li_fila] = dw_4.Object.espe_codigo[li_i]
			dw_pallets.Object.vari_codigo[li_fila] = dw_4.Object.vari_codigo[li_i]
			dw_pallets.Object.cond_codigo[li_fila] = dw_4.Object.condicion[li_i]
			dw_pallets.Object.etiq_codigo[li_fila] = dw_4.Object.etiq_codigo[li_i]
			dw_pallets.Object.prod_codigo[li_fila] = dw_4.Object.productor[li_i]
			dw_pallets.Object.emba_codigo[li_fila] = dw_4.Object.emba_codigo[li_i]
			dw_pallets.Object.pafr_calibr[li_fila] = dw_4.Object.pafr_calibr[li_i]
			dw_pallets.Object.pafr_secuen[li_fila] = dw_4.Object.secuencia[li_i]
			dw_pallets.Object.dest_codigo[li_fila] = dw_4.Object.dest_codigo[li_i]
			dw_pallets.Object.stat_codigo[li_fila] = dw_4.Object.stat_codigo[li_i]
			dw_pallets.Object.inpe_codigo[li_fila] = dw_4.Object.inpe_numero[li_i]
			
			dw_4.SelectRow(li_i, Not(dw_4.IsSelected(li_i)))
			
			dw_4.Object.color[li_i] = 1
			
			IF ii_tipo = 1 THEN
				dw_pallets.Object.dece_tipsel[li_fila] = 1
			ELSE
				dw_pallets.Object.dece_tipsel[li_fila] = 2
			END IF	
			
			IF dw_pallets.RowCount() > 0 THEN
				pb_grabar.Enabled = True
			END IF	
		ELSE
			MESSAGEBOX("CONTROL", "Pallet " + string(li_pallet) + "-" + string(li_secuen) + " Ya existe en la lista de pallets seleccionados")
			dw_4.SelectRow(li_i, Not(dw_4.IsSelected(li_i)))
			dw_4.Object.color[li_i] = 1
		END IF
	END IF
NEXT
end event

type cb_elimina from uo_cbstandard within w_mant_instructivo_emb
integer x = 567
integer y = 1580
integer width = 137
integer height = 60
integer taborder = 21
boolean bringtotop = true
string text = ">>"
end type

event clicked;call super::clicked;Long li_i, li_selecionadas, ll_pallet, ll_fila

FOR li_i =  1 to dw_pallets.RowCount()
	IF dw_pallets.IsSelected(li_i) THEN
		ll_pallet = dw_pallets.Object.paen_numero[li_i]
		FOR ll_fila = 1 TO dw_4.RowCount()
								
			IF dw_4.Find("pallet = " + string(ll_pallet), ll_fila, ll_fila) > 0 THEN
				dw_4.Object.color[ll_fila] = 0
			END IF	
			
			
		NEXT	
	END IF	
NEXT


li_i = 1

FOR li_i =  1 to dw_pallets.RowCount()

	IF dw_pallets.IsSelected(li_i) THEN
		
		li_selecionadas = li_selecionadas + 1
		dw_pallets.DeleteRow(li_i)		
		li_i = li_i -1
	END IF
	
NEXT


end event

type dw_4 from uo_dw within w_mant_instructivo_emb
integer x = 709
integer y = 1120
integer width = 2569
integer height = 944
integer taborder = 100
boolean titlebar = true
string title = "Pallets Disponibles"
string dataobject = "dw_info_instructivo_emb_pallets"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;Long	ll_fila, ll_pallet

IF row > 0 THEN
	FOR ll_fila = 1 TO dw_4.RowCount()
		
		ll_pallet = dw_4.Object.pallet[row]
		
		IF dw_4.Find("pallet = " + string(ll_pallet), ll_fila, ll_fila) > 0 THEN
			//THIS.IsSelected(ll_fila)
			THIS.SelectRow(ll_fila, Not(THIS.IsSelected(ll_fila)))
		END IF	
	NEXT	
ELSE
	THIS.SelectRow(row, Not(THIS.IsSelected(row)))
END IF	






end event

event getfocus;call super::getfocus;this.Title = "Pallets Disponibles"
dw_pallets.Title = ""
end event

event losefocus;call super::losefocus;this.Title = ""
end event

event retrievestart;call super::retrievestart;//Return 2
end event

type dw_3 from uo_dw within w_mant_instructivo_emb
boolean visible = false
integer x = 722
integer y = 1124
integer width = 2546
integer height = 944
integer taborder = 200
boolean titlebar = true
string title = "dw_3"
string dataobject = "dw_info_instructivo_emb_pallets"
boolean livescroll = true
end type

event clicked;call super::clicked;IF THIS.IsSelected(row) THEN
	THIS.SelectRow(row, FALSE)
ELSE
	THIS.SelectRow(row, TRUE)
END IF
end event

type dw_5 from datawindow within w_mant_instructivo_emb
boolean visible = false
integer x = 1504
integer y = 2252
integer width = 1074
integer height = 568
integer taborder = 31
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_despaselecsele2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from uo_dw within w_mant_instructivo_emb
boolean visible = false
integer x = 3442
integer y = 2084
integer width = 535
integer height = 944
integer taborder = 21
boolean bringtotop = true
boolean titlebar = true
string title = "Pallets Selec."
string dataobject = "dw_mant_despaselecsele"
boolean hscrollbar = true
end type

event clicked;call super::clicked;THIS.SelectRow(row, Not(THIS.IsSelected(row)))
end event

event getfocus;call super::getfocus;this.Title = "Pallets Selec."
dw_4.Title = ""
end event

event losefocus;call super::losefocus;this.Title = ""
end event

