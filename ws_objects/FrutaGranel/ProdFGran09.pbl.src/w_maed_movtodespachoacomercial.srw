$PBExportHeader$w_maed_movtodespachoacomercial.srw
$PBExportComments$Ventana que genera recepción de lotes en Fruta Comercial
forward
global type w_maed_movtodespachoacomercial from w_mant_encab_deta_csd
end type
type cb_guia from commandbutton within w_maed_movtodespachoacomercial
end type
end forward

global type w_maed_movtodespachoacomercial from w_mant_encab_deta_csd
integer width = 5038
integer height = 1920
string title = "Traspaso de Fruta a Comercial"
string menuname = ""
cb_guia cb_guia
end type
global w_maed_movtodespachoacomercial w_maed_movtodespachoacomercial

type variables
DataWindowChild   idwc_PltaDest, idwc_tipomov, idwc_Camara, idwc_PltaLote, idwc_Especie, idwc_variedades, idwc_productor

uo_plantadesp		iuo_PltaDestino
uo_tipomovtofruta	iuo_TipoMovtoFruta
uo_cliente        	iuo_cliente
uo_plantadesp		iuo_Planta
uo_especie			iuo_Especie

Long     				il_NumFruta	=	0
Integer 				ii_solori, ii_sentido, ii_tipodoc
Boolean				ib_AutoCommit, ib_transporte = TRUE
String					is_rut, is_rutprod, is_RutProductor, is_NombreProductor, is_columna[]
Integer				ii_Cantidad, ii_Productor

w_mant_deta_movtofrutacomercial	iw_mantencion_1
end variables

forward prototypes
public subroutine habilitaenca (boolean habilita)
public function boolean noexistetipomov (integer ai_tipomov)
public function boolean verificalotes ()
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean verificaborrar (integer ai_fila)
public subroutine habilitaingreso (string as_columna)
public function boolean existemovimiento (integer ai_planta, integer ai_tipomovto, long al_numero)
end prototypes

public subroutine habilitaenca (boolean habilita);IF Habilita THEN
	dw_2.Object.mfge_numero.Protect			=	0
//	dw_2.Object.mfge_fecmov.Protect				=	0	
//	dw_2.Object.refg_horasa.Protect				=	0
	dw_2.Object.moti_codigo.Protect				=	0
	dw_2.Object.mfge_observ.Protect				=	0
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.espe_codigo.Protect				=	0
	
	dw_2.Object.mfge_numero.Color	=	0
//	dw_2.Object.mfge_fecmov.Color	=	0
//	dw_2.Object.refg_horasa.Color		=	0
//	dw_2.Object.moti_codigo.Color		=	0
	dw_2.Object.mfge_observ.Color	=	0
	dw_2.Object.clie_codigo.Color		=	0
	dw_2.Object.espe_codigo.Color	=	0
	
	dw_2.Object.mfge_numero.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.mfge_fecmov.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.refg_horasa.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.moti_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_observ.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_2.Object.mfge_numero.Protect			=	0
//	dw_2.Object.mfge_fecmov.Protect				=	0	
//	dw_2.Object.refg_horasa.Protect				=	0
	dw_2.Object.moti_codigo.Protect				=	0
	dw_2.Object.mfge_observ.Protect				=	0
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.espe_codigo.Protect				=	0
	
	dw_2.Object.mfge_numero.Color	=	RGB(255,255,255)
//	dw_2.Object.mfge_fecmov.Color	=	RGB(255,255,255)
//	dw_2.Object.refg_horasa.Color		=	RGB(255,255,255)
	dw_2.Object.moti_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_observ.Color	=	RGB(255,255,255)
	dw_2.Object.clie_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.espe_codigo.Color	=	RGB(255,255,255)
	
	dw_2.Object.mfge_numero.BackGround.Color	=	553648127
//	dw_2.Object.mfge_fecmov.BackGround.Color	=	553648127
//	dw_2.Object.refg_horasa.BackGround.Color		=	553648127
	dw_2.Object.moti_codigo.BackGround.Color		=	553648127
	dw_2.Object.mfge_observ.BackGround.Color	=	553648127
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
END IF
end subroutine

public function boolean noexistetipomov (integer ai_tipomov);Integer	li_Contador, li_soltra
String	ls_nombre

SELECT	tpmv_nombre, tpmv_solori, tpmv_sentid, tdop_codigo, tpmv_soltra
	INTO	:ls_nombre, : ii_solori, :ii_sentido, :ii_tipodoc, :li_soltra
	FROM	dbo.spro_tipomovtofruta
	WHERE	tpmv_codigo	=	:ai_tipomov
	AND	tpmv_frugra	=	0
	AND	tpmv_fruemb	=	0
	AND	tpmv_frucom	=	0
	AND	tpmv_envase	=	1;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla Tipo de Movimiento de Fruta")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de Tipo de Movimiento de Fruta (" + String(ai_tipomov, '000') + "), no ha sido~r" + &
		"ingresado o no pertenece a Tipo de Movimiento de Envases.~r~rIngrese o seleccione otro Código.")
		
	RETURN True
END IF

IF li_soltra = 1 THEN
	ib_transporte = TRUE
ELSE
	ib_transporte = FALSE
END IF	

RETURN False
end function

public function boolean verificalotes ();//Long ll_fila
//Integer li_Cantidad, li_loteplt, li_loteesp, li_lotecod, li_respuesta
//
//FOR ll_fila = 1 TO dw_1.RowCount()
//	
//	li_loteplt = dw_1.Object.lote_pltcod[ll_fila]
//	li_loteesp = dw_1.Object.lote_espcod[ll_fila]
//	li_lotecod = dw_1.Object.lote_codigo[ll_fila]
//	
//	SELECT Count(*) INTO :li_cantidad
//	  FROM dbo.spro_lotesfrutacomenc
//	 WHERE lofc_pltcod = :li_loteplt
//	   AND lofc_espcod = :li_loteesp
//		AND lofc_lotefc = :li_lotecod;
//		
//	IF Isnull(li_cantidad) THEN li_cantidad = 0 
//	
//	IF li_cantidad > 0 THEN
//		li_respuesta = MessageBox("Atención","El lote " + String(li_lotecod,'0000') + " ya se encuentra Ingresado en Mercado Interno." + &
//		                          "~r Desea Incorporarlo a la secuencia del lote Comercial?." + &
//										  "~r~r Si responde 'No' debera eliminarlo del Detalle antes de grabar.", &
//												Information!, YesNo!)
//		IF li_respuesta = 2 THEN										
//			Return FALSE
//		END IF	
//	END IF
//NEXT	
//
RETURN TRUE
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF NOT Borrando THEN
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_2.SetItem(1,"mfge_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"mfge_comput",gstr_us.computador)
		dw_2.SetItem(1,"mfge_horacr",F_Fechahora())
	ELSEIF dw_2.GetItemStatus(1, 0, Primary!) = DataModified! THEN
		dw_2.SetItem(1,"mfge_usumod",gstr_us.nombre)
		dw_2.SetItem(1,"mfge_commod",gstr_us.computador)
		dw_2.SetItem(1,"mfge_horact",F_Fechahora())
		dw_2.SetItem(1,"mfge_estmov", 0)
	END IF
END IF

IF Borrando THEN
	IF dw_1.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
	ELSEIF dw_2.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	ELSE
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
		ELSE
			lb_Retorno	=	True
			
			dw_2.ResetUpdate()
			dw_1.ResetUpdate()
		END IF
	END IF
ELSE
	IF dw_2.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
	ELSEIF dw_1.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	ELSE
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
		ELSE
			lb_Retorno	=	True
			
			dw_2.ResetUpdate()
			dw_1.ResetUpdate()
			
		END IF
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean verificaborrar (integer ai_fila);Long ll_fila
Integer li_Cantidad, li_loteplt, li_loteesp, li_lotecod, li_respuesta, li_tipoenv, li_envase

li_loteplt = dw_1.Object.lote_pltcod[ai_fila]
li_loteesp = dw_1.Object.lote_espcod[ai_fila]
li_lotecod = dw_1.Object.lote_codigo[ai_fila]
li_tipoenv = dw_1.Object.enva_tipoen[ai_fila]
li_envase  = dw_1.Object.enva_codigo[ai_fila]

SELECT Count(Distinct mcd.mfco_numero) INTO :li_cantidad
  FROM dbo.spro_movtofrutacomdeta mcd, dbo.spro_lotesfrutacomdeta lfc
 WHERE lfc.lofc_pltcod = :li_loteplt
	AND lfc.lofc_espcod = :li_loteesp
	AND lfc.lofc_lotefc = :li_lotecod
	AND lfc.enva_tipoen = :li_tipoenv
	AND lfc.enva_codigo = :li_envase
	AND mcd.lofc_pltcod = lfc.lofc_pltcod
	AND mcd.lofc_espcod = lfc.lofc_espcod
	AND mcd.lofc_lotefc = lfc.lofc_lotefc
	AND mcd.lfcd_secuen = lfc.lfcd_secuen
	AND mcd.tpmv_codigo <>9;
		
IF Isnull(li_cantidad) THEN li_cantidad = 0 
	
IF li_cantidad > 0 THEN
	MessageBox("Atención","El lote " + String(li_lotecod,'0000') + " se encuentra con movimientos en Fruta Comercial. Imposible Eliminar.")
	Return FALSE
END IF


RETURN TRUE
end function

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date	ldt_Fecha

IF as_Columna <> "mfge_fecmov" AND &
	(dw_2.Object.mfge_fecmov[1] = ldt_Fecha OR IsNull(dw_2.Object.mfge_fecmov[1])) THEN
	lb_Estado = False
END IF
	

IF as_Columna <> "moti_codigo" AND &
	(dw_2.Object.moti_codigo[1] = 0 OR IsNull(dw_2.Object.moti_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "espe_codigo" AND &
	(dw_2.Object.espe_codigo[1] = 0 OR IsNull(dw_2.Object.espe_codigo[1])) THEN
	lb_Estado = False
END IF

dw_1.Enabled			=	lb_Estado

IF isnull(dw_2.Object.mfco_numrec[1]) THEN
	pb_ins_det.Enabled	=	lb_Estado
END IF

end subroutine

public function boolean existemovimiento (integer ai_planta, integer ai_tipomovto, long al_numero);Long		ll_Numero
Boolean	lb_Retorno = True

 SELECT 	mfge_numero
	INTO	:ll_Numero
	FROM	dbo.spro_movtofrutagranenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfge_numero	=	:al_Numero;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Fruta Granel")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.Argumento[3]	=	String(al_Numero)
	This.TriggerEvent("ue_recuperadatos")
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

event open;/* 
	Argumentos
		istr_Mant.Argumento[1]		=	Código Planta
		istr_Mant.Argumento[2]		=	Tipo de Movimiento
		istr_Mant.Argumento[3]		=	Número de Movimiento	
		istr_Mant.Argumento[4]		=	Sentido del Movimiento => 2 = Despacho o Salida; 1 = Entrada
		istr_Mant.Argumento[5]  	=  
		istr_Mant.Argumento[6]		=	
		istr_Mant.Argumento[13]		=	Cliente
*/

x				= 0   
y				= 0
This.Height	= 2500
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True


istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	"36" 
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	"2"
istr_Mant.Argumento[10]	=	""
istr_Mant.Argumento[10]	=	String(gi_CodExport)

dw_2.GetChild("tpmv_codigo", idwc_tipomov)
idwc_tipomov.SetTransObject(sqlca)

IF idwc_tipomov.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Tipo de Movimiento")
	idwc_tipomov.InsertRow(0)
END IF


dw_1.GetChild("cama_codigo", idwc_Camara)
idwc_Camara.SetTransObject(sqlca)

IF idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	idwc_Camara.InsertRow(0)
ELSE
	idwc_Camara.SetSort("cama_nombre A")
	idwc_Camara.Sort()
END IF

dw_1.GetChild("lote_pltcod", idwc_PltaLote)
idwc_PltaLote.SetTransObject(sqlca)
idwc_PltaLote.Retrieve()

dw_1.GetChild("lote_espcod", idwc_Especie)
idwc_Especie.SetTransObject(sqlca)
idwc_Especie.Retrieve()


dw_1.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(sqlca)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

istr_Mant.dw				=	dw_1
istr_Mant.solo_consulta 	=	False

pb_nuevo.PostEvent(Clicked!)

iuo_PltaDestino			=	Create uo_plantadesp
iuo_TipoMovtoFruta	=	Create uo_tipomovtofruta				
iuo_especie				=	Create uo_especie

end event

on w_maed_movtodespachoacomercial.create
int iCurrent
call super::create
this.cb_guia=create cb_guia
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_guia
end on

on w_maed_movtodespachoacomercial.destroy
call super::destroy
destroy(this.cb_guia)
end on

event ue_nuevo;
is_rut 		=	""
is_rutprod	=	""

Call Super::ue_nuevo

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	Integer(istr_Mant.Argumento[2])
dw_2.Object.mfge_fecmov[1]	=	Date(Today())
dw_2.Object.refg_horasa[1]	=	Time(Today())
dw_2.Object.clie_codigo[1]		=	gi_CodExport

istr_Mant.Argumento[1]		=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[3]		=	""
istr_Mant.Argumento[4]		=	"2"
istr_Mant.Argumento[13]	=	String(gi_CodExport)

Habilitaenca(True)

end event

event ue_recuperadatos;Long		ll_fila_e, respuesta, ll_fila_env
Integer	li_Sentido

DO
	

	dw_1.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(sqlca)
	idwc_productor.Retrieve(-1) 
	idwc_productor.InsertRow(0)
	
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(	Integer(istr_mant.argumento[1])	, &
										 	Integer(istr_mant.argumento[2])	, &
										  	Long(istr_mant.argumento[3])		, &
						  				 	Integer(istr_mant.argumento[13]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila_e > 0 THEN
				
		HabilitaEnca(False)

		DO			
			IF dw_1.Retrieve(Integer(istr_mant.argumento[1]),&
							     Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),&
						  			Integer(istr_mant.argumento[13])) = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled  	= True
				pb_grabar.Enabled	= True
				pb_ins_det.Enabled	= True
				pb_imprimir.Enabled	= True
				pb_eli_det.Enabled	= True
				
				IF NOT isnull(dw_2.Object.mfco_numrec[1]) THEN
					pb_ins_det.Enabled = FALSE
				END IF
				
				dw_1.SetRow(1)
				dw_1.SelectRow(1,True)
				dw_1.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

end event

event ue_antesguardar;Long		ll_fila, ll_guia
Boolean  lb_Actualiza_Fruta = FALSE			
Integer	li_Planta, li_TipoMovto, li_Secuencia

Message.DoubleParm = 0

If NOT VerIficaLotes() Then
	Message.DoubleParm = -1
	RETURN
End If	

If dw_1.RowCount() > 0 Then
	If dw_1.Object.total_bultos[dw_1.RowCount()] > 9999 Then
		Messagebox("Error de Consistencia","Total de bultos supera lo permitido")
		Message.DoubleParm = -1
		RETURN
	End If 
End If

li_Planta			=	Integer(istr_mant.Argumento[1])
li_TipoMovto	=	Integer(istr_mant.Argumento[2])

ib_AutoCommit		=	SQLCA.AutoCommit
SQLCA.AutoCommit	=	False

If dw_2.GetItemStatus(1, 0, Primary!) = NewModIfied! Then
	If il_NumFruta=0 Then
		iuo_TipoMovtoFruta.bloqueacorrel()
		il_NumFruta = iuo_TipoMovtoFruta.UltimoCorrelativo(1,li_TipoMovto,li_Planta) 
	
		If il_NumFruta = 0 OR IsNull(il_NumFruta) Then
			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			Message.DoubleParm = -1
			RETURN
		Else
			lb_Actualiza_Fruta = TRUE	
			
		End If
   End If
	
	dw_2.Object.mfge_numero[1]	=	il_NumFruta
	istr_mant.Argumento[3]		=	String(il_NumFruta)
	
	  	//Preguntar el Momento de Actualización
   If lb_Actualiza_Fruta  Then iuo_TipoMovtoFruta.Actualiza_Correlativo(1,li_Planta,li_TipoMovto,il_NumFruta) 
      ///////////////////////////////////////
Else
	il_NumFruta					= 	dw_2.Object.mfge_numero[1]
	istr_mant.Argumento[3]	=	String(dw_2.Object.mfge_numero[1])
End If

If dw_1.RowCount() > 0 Then
   dw_2.Object.mfge_totbul[1] = dw_1.Object.total_bultos[dw_1.RowCount()]
	dw_2.Object.mfge_tpneto[1] = dw_1.Object.total_kilos[dw_1.RowCount()]
Else
	dw_2.Object.mfge_totbul[1] = 0
	dw_2.Object.mfge_tpneto[1] = 0
End If	

SELECT	IsNull(Max(mfgd_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dbo.spro_movtofrutagrandeta
	WHERE	plde_codigo	=	:li_Planta
	AND	tpmv_codigo	=	:li_TipoMovto
	AND	mfge_numero	=	:il_NumFruta ;
	

FOR ll_Fila = 1 TO dw_1.RowCount()
	If dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied! Then
		
		dw_1.Object.plde_codigo[ll_Fila]		=	dw_2.Object.plde_codigo[1]
		dw_1.Object.tpmv_codigo[ll_Fila]		=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
		dw_1.Object.clie_codigo[ll_Fila]		=	dw_2.Object.clie_codigo[1]
		dw_1.Object.mfgd_secuen[ll_Fila]		=	li_Secuencia
		
		li_Secuencia ++
	End If
NEXT
end event

event ue_modifica_detalle;Boolean lb_SoloConsulta

istr_mant.agrega	= False
istr_mant.borra	= False


IF dw_1.RowCount() > 0 THEN
	istr_mant.dw	=	dw_1
	
	lb_soloconsulta = istr_mant.Solo_Consulta
	
	IF NOT isnull(dw_2.Object.mfco_numrec[1]) THEN
		istr_mant.Solo_Consulta = TRUE
	END IF
	
	istr_mant.Argumento[14]	=	String(dw_1.Object.vari_codigo[dw_1.GetRow()])
	
	OpenWithParm(iw_mantencion_1, istr_mant)
	
	istr_mant.Solo_Consulta = lb_SoloConsulta
	
END IF


end event

event ue_nuevo_detalle;
If IsNull(dw_2.Object.mfco_numrec[1]) Then
	istr_mant.Borra		=	False
	istr_mant.Agrega	=	True

	istr_mant.dw	=	dw_1
	OpenWithParm(iw_mantencion_1, istr_mant)
	
	If dw_1.RowCount() > 0  Then
		HabilitaEnca(FALSE)
		pb_eliminar.Enabled	=	True
		pb_grabar.Enabled	=	True
		pb_eli_det.Enabled	=	True
	Else
		pb_eliminar.Enabled	=	False
		pb_grabar.Enabled	=	False
		pb_eli_det.Enabled	=	False
	End If
Else
	pb_ins_det.Enabled 		= False
End If
end event

event ue_borra_detalle;call super::ue_borra_detalle;Integer	li_borra

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")
If Message.DoubleParm = -1 Then RETURN

If Not VerIficaBorrar(il_fila) Then
	Message.DoubleParm = -1
	RETURN
End If

istr_mant.Borra		= True
istr_mant.Agrega	= False

istr_mant.dw	=	dw_1

OpenWithParm(iw_mantencion_1, istr_mant)

istr_mant = Message.PowerObjectParm

If istr_mant.respuesta = 1 Then
	li_borra	=	dw_1.DeleteRow(0)
	If li_borra = 1 Then
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	Else
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	End If
End If

istr_mant.borra	 = False

If dw_1.RowCount()<=0 Then
	pb_eliminar.Enabled	=	False
	pb_grabar.Enabled		=	False
	pb_eli_det.Enabled	=	False
End If	
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "DESPACHO A COMERCIAL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_traspasofrutacomercial"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(Dec(istr_mant.argumento[1]), Dec(istr_mant.argumento[2])	, &
										  	Dec(istr_mant.argumento[3]),  Dec(istr_mant.argumento[13]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.",StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_busq

DateTime			ld_FechaInicio

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[1]
lstr_Busq.Argum[2]	=	istr_Mant.Argumento[2]
lstr_Busq.Argum[3]	=	'3'
lstr_Busq.Argum[4]	=	String(ld_FechaInicio,'dd/mm/yyyy')
lstr_Busq.Argum[10]	=	String(dw_2.Object.clie_codigo[1])

OpenWithParm(w_busc_spro_movtofrutagranenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	istr_Mant.Argumento[1]	=	lstr_Busq.Argum[1]
	istr_Mant.Argumento[2]	=	lstr_Busq.Argum[2]
	istr_Mant.Argumento[3]	=	lstr_Busq.Argum[3]
	
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_guardar;If dw_1.AcceptText() = -1 Then RETURN

Integer li_planta, li_tipomovto, li_cliente
Long    ll_numero
SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

If Message.DoubleParm = -1 Then RETURN

If wf_actualiza_db(False) Then
	w_main.SetMicroHelp("Información Grabada.")
	
	If isnull(dw_2.Object.mfco_numrec[1]) Then
		li_planta    	= 	dw_2.Object.plde_codigo[1]
		li_tipomovto	= 	dw_2.Object.tpmv_codigo[1]
		ll_numero    = 	dw_2.Object.mfge_numero[1]
		li_cliente		=	dw_2.Object.clie_codigo[1]
		
		DECLARE GeneraRecepcion PROCEDURE FOR dbo.FGran_GeneraTraspasoComercial
			@Planta		=	:li_Planta	,   
			@TipoMovto	=	:li_TipoMovto,   
			@Numero	=	:ll_Numero,
			@Cliente		=	:li_cliente;
		
		EXECUTE GeneraRecepcion;
					
		If sqlca.SQLCode = -1 Then
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack ;
			
			w_main.SetMicroHelp("No se puede Grabar información.")
			Message.DoubleParm = -1
			RETURN
		Else
			Commit;
		End If
		
		Close GeneraRecepcion ;
		
		dw_2.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2])	,&
						  	  Long(istr_mant.argumento[3]),Integer(istr_mant.argumento[13]))
				  
	End If	
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
End If

If il_NumFruta > 0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModIfied! Then
	il_NumFruta	= 0
End If
end event

event ue_borrar;Integer li_fila, li_cliente, li_tipomovto, ll_numero

IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

FOR li_fila = 1 TO dw_1.RowCount()
	IF NOT verificaborrar(li_fila) THEN
		Message.DoubleParm = -1
		RETURN
	END IF
NEXT

IF Message.DoubleParm = -1 THEN RETURN

li_cliente    = dw_2.Object.clie_codigo[1]
li_tipomovto = dw_2.Object.tpmv_codrec[1]
ll_numero    = dw_2.Object.mfco_numrec[1]
		
IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
			
			DECLARE GeneraEliminación PROCEDURE FOR dbo.FGran_EliminaTraspasoComercial
				@Cliente			=	:li_cliente,   
				@TipoMovto		=	:li_TipoMovto,   
				@Numero		=	:ll_Numero;
		
			EXECUTE GeneraEliminación;
					
			IF sqlca.SQLCode = -1 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack ;
			END IF
			
			CLOSE GeneraEliminación;
			
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

event ue_validaborrar;Integer li_cantidad
Integer li_fila, li_cliente, li_tipomovto, ll_numero

w_main.SetMicroHelp("Validando movimientos de comercial...")

li_cliente    = dw_2.Object.clie_codigo[1]
li_tipomovto = dw_2.Object.tpmv_codrec[1]
ll_numero    = dw_2.Object.mfco_numrec[1]

IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN

	DECLARE ValidaEliminación PROCEDURE FOR dbo.FGran_ValidaEliminaTraspasoComercial
		@Cliente		 	=	:li_cliente,   
		@Tipo			=	:li_TipoMovto,   
		@Numero		=	:ll_Numero;
	
	EXECUTE ValidaEliminación;
	
	FETCH ValidaEliminación INTO :li_cantidad;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Fruta Comercial")
		Message.DoubleParm = -1
	ELSEIF li_cantidad = 0 OR IsNull(li_cantidad) THEN
		Message.DoubleParm = 1
	ELSE
		MessageBox("Atención","No se puede eliminar movimiento, ya que esta asociado a otros en Fruta Comercial.")
		Message.DoubleParm = -1
	END IF
	
	CLOSE ValidaEliminación;
	
ELSE
	Message.DoubleParm = -1
END IF

RETURN
end event

event resize;call super::resize;
cb_guia.x 							=  pb_salir.x
cb_guia.y 							=  pb_salir.y + pb_salir.Height
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtodespachoacomercial
integer x = 55
integer y = 600
integer width = 4635
integer height = 1192
string title = "Detalle Movimiento de Traspaso a Comercial"
string dataobject = "dw_mues_movtofrutagraneldeta_comercial"
end type

event type long dw_1::dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtoenvases.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtoenvases.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event dw_1::losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event dw_1::clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtodespachoacomercial
integer x = 832
integer y = 60
integer width = 3049
integer height = 480
string dataobject = "dw_mant_movtofrutagranel_despacho_comer"
end type

event dw_2::itemchanged;Integer	li_Null
String	ls_Columna, ls_Nula

ls_Columna = dwo.Name
SetNull(li_Null)
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
		
	CASE "mfge_numero"
		IF NOT ExisteMovimiento(gstr_ParamPlanta.CodigoPlanta, integer(istr_mant.argumento[2]), Integer(data)) THEN
			This.SetItem(1,"mfge_numero", li_Null)
			This.SetFocus()
			RETURN 1
		END IF	
		
   CASE "tpmv_codigo"
		IF NoExisteTipoMov(Integer(Data)) THEN
			This.SetItem(1, ls_Columna, li_Null)
			RETURN 1
			
		ElSE
			istr_mant.argumento[2] = String(Integer(Data))
			istr_mant.argumento[4] = string(ii_sentido)
			istr_mant.argumento[5] = string(ii_solori)
			
			RETURN 0
			
		END IF
	
	CASE "clie_codigo"
		IF ExisteCliente(Integer(data), is_columna[]) THEN
			istr_mant.argumento[13]	=	data
		ELSE
			This.SetItem(1, "clie_codigo", li_Null)
			RETURN 1
		END IF
	
	CASE "espe_codigo"
		IF iuo_especie.Existe(Integer(data), TRUE, sqlca) THEN
			istr_mant.argumento[14]	=	data
			idwc_variedades.Retrieve(integer(data))
		ELSE
			This.SetItem(1, "espe_codigo", li_Null)
			RETURN 1
		END IF
END CHOOSE

HabilitaIngreso(ls_Columna)
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	This.Object.meen_rutcho.Format = '@@@.@@@.@@@-@'
	
	IF dwo.Name <> "meen_rutcho" THEN
		This.SetItem(1, "meen_rutcho", is_rut)
	END IF
END IF

IF is_rutprod <> "" THEN
	This.Object.prod_rut.Format = '@@@.@@@.@@@-@'
	
	IF dwo.Name <> "prod_rut" THEN
		This.SetItem(1, "prod_rut", is_rutprod)
	END IF
END IF
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtodespachoacomercial
integer x = 4718
integer y = 252
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtodespachoacomercial
integer x = 4718
integer y = 444
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtodespachoacomercial
integer x = 4718
integer y = 632
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtodespachoacomercial
integer x = 4718
integer y = 820
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtodespachoacomercial
integer x = 4718
integer y = 1012
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtodespachoacomercial
integer x = 4718
integer y = 1352
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtodespachoacomercial
integer x = 4718
integer y = 1540
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtodespachoacomercial
integer x = 4718
integer y = 60
end type

type cb_guia from commandbutton within w_maed_movtodespachoacomercial
boolean visible = false
integer x = 4718
integer y = 1228
integer width = 302
integer height = 112
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Guía SII"
end type

event clicked;Long		ll_Fila, ll_Fila_Busca
str_mant	lstr_mant

lstr_Mant.Argumento[1]	=	String(dw_2.Object.plde_codigo[1])
lstr_Mant.Argumento[2]	=	String(dw_2.Object.tpmv_codigo[1])
lstr_Mant.Argumento[3]	=	String(dw_2.Object.meen_numero[1])
		
OpenWithParm(w_emis_guia_despacho_envases, lstr_Mant)

lstr_Mant = Message.PowerObjectParm

IF lstr_mant.Respuesta = 1 THEN
	parent.Triggerevent("ue_recuperadatos")
	this.Enabled = FALSE
END IF	
end event

