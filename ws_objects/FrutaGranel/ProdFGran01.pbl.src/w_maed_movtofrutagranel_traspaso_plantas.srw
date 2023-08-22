$PBExportHeader$w_maed_movtofrutagranel_traspaso_plantas.srw
$PBExportComments$Proceso de Fruta Granel traspaso Interplanta
forward
global type w_maed_movtofrutagranel_traspaso_plantas from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_movtofrutagranel_traspaso_plantas
end type
type tp_1 from userobject within tab_1
end type
type dw_detalle from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_detalle dw_detalle
end type
type tp_2 from userobject within tab_1
end type
type dw_envases from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_envases dw_envases
end type
type tab_1 from tab within w_maed_movtofrutagranel_traspaso_plantas
tp_1 tp_1
tp_2 tp_2
end type
type dw_3 from datawindow within w_maed_movtofrutagranel_traspaso_plantas
end type
end forward

global type w_maed_movtofrutagranel_traspaso_plantas from w_mant_encab_deta_csd
integer width = 3278
integer height = 2048
string title = "DESPACHO DE FRUTA GRANEL A PROCESO"
string menuname = ""
windowstate windowstate = maximized!
event ue_imprimir ( )
tab_1 tab_1
dw_3 dw_3
end type
global w_maed_movtofrutagranel_traspaso_plantas w_maed_movtofrutagranel_traspaso_plantas

type variables
w_mant_deta_movtofrutagranel_despacho	iw_mantencion_1
w_mant_deta_movtoenvadeta					iw_mantencion_2

uo_plantadesp			iuo_Packing
uo_motivodespacho		iuo_MotiDespacho
uo_transportista		iuo_Transport
uo_camiones				iuo_Camion
uo_tipodoctoplanta	iuo_TipoDocto

Boolean							ib_Modifica, ib_AutoCommit
DataWindowChild   			idwc_PltaDest, idwc_Motivo, idwc_Transp, idwc_Camion, &
									idwc_Camara, idwc_TipoEnvase, idwc_PltaLote, idwc_Especie, &
									idwc_Packing, idwc_Envases
DataWindow						dw_4, dw_5
str_variedad					istr_variedad
str_categoria					istr_categoria

String							is_rut, is_columna,is_rutemp,is_sermed
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existedocproceso (integer ai_planta, integer ai_numero)
public function boolean insertadetallegranel ()
public function boolean revisaenvases ()
public function boolean existedespacho (integer ai_planta, integer ai_tipomovto, long al_numero)
public subroutine habilitaingreso (string as_columna)
end prototypes

event ue_imprimir();SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "DESPACHO A PROCESOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_movtofruta_proceso_enca"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[1]), integer(istr_mant.argumento[2]), &
                          integer(istr_mant.argumento[3]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila =0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.mfge_numero.Protect				=	0
	dw_2.Object.mfge_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.defg_docrel.Protect				=	0
	dw_2.Object.defg_docrel.BackGround.Color	=	RGB(255,255,255)
	//dw_2.Object.plde_coorde.Protect				=	0
	//dw_2.Object.plde_coorde.BackGround.Color	=	RGB(255,255,255)	
ELSE
	dw_2.Object.mfge_numero.Protect				=	1
	dw_2.Object.mfge_numero.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.defg_docrel.Protect				=	1
	dw_2.Object.defg_docrel.BackGround.Color	=	RGB(192,192,192)
	//dw_2.Object.plde_coorde.Protect				=	1
	//dw_2.Object.plde_coorde.BackGround.Color	=	RGB(192,192,192)

END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean			lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF Borrando THEN
	IF dw_5.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				IF dw_2.Update(True, False) = 1 THEN
					Commit;
			
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
					ELSE
						lb_Retorno	=	True
			
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						dw_4.ResetUpdate()
						dw_5.ResetUpdate()
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
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)

		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_4.Update(True, False) = 1 THEN
			IF dw_1.Update(True, False) = 1 THEN
				IF dw_5.Update(True, False) = 1 THEN
					Commit;
			
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
					ELSE
						lb_Retorno	=	True
			
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						dw_4.ResetUpdate()
						dw_5.ResetUpdate()
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
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)

		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public function boolean existedocproceso (integer ai_planta, integer ai_numero);Long		ll_Numero,ll_Productor 
Integer	li_Especie, li_Variedad, li_Cantidad, li_Vigencia
String   ls_Nombre
Boolean	lb_Retorno = True

SELECT	orpr_numero, prod_codigo, espe_codigo, vari_codigo, orpr_canbul, &
			orpr_estado
  INTO	:ll_Numero, :ll_Productor, :li_Especie, :li_Variedad, :li_Cantidad, &
  			:li_Vigencia
	FROM	dba.spro_ordenproceso
	WHERE	plde_codigo		=	:ai_Planta
	AND	orpr_tipord		=	1
	And   orpr_numero    =	:ai_Numero ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Ordenes de Proceso")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	IF li_Vigencia <> 1 THEN
		MessageBox("Atención","Número de Orden de Proceso no se encuentra vigente. Ingrese Otro.")
		lb_Retorno	=	False	
   ELSE
		istr_mant.Argumento[5] 	=	String(ll_Numero)
		istr_mant.Argumento[6]  = 	String(ll_Productor)
		istr_mant.Argumento[7]  = 	''
		istr_mant.Argumento[8]  = 	String(li_Especie)
		istr_mant.Argumento[9]  = 	String(li_Variedad)
		istr_mant.Argumento[10] = 	''
		istr_mant.Argumento[11] = 	String(li_Cantidad)
	
		IF ll_Productor <> 0 OR Not(IsNull(ll_Productor)) THEN
   
			SELECT	prod_nombre
				INTO	:ls_Nombre
				FROM	dba.productores
				WHERE	prod_codigo	=	:ll_Productor;
	    
     		 IF sqlca.SQLCode = 0 THEN
				istr_mant.Argumento[7]	=	ls_Nombre
			END IF	
   	END IF	
	
		IF li_Variedad <> 0 OR Not(IsNull(li_Variedad)) THEN
   		SELECT	vari_nombre
				INTO	:ls_Nombre
				FROM	dba.variedades
				WHERE	espe_codigo	=	:li_Especie
				AND   vari_codigo	=  :li_Variedad;
	    
      	IF sqlca.SQLCode = 0 THEN
				istr_mant.Argumento[10]	=	ls_Nombre
			END IF	
   	END IF
		
		dw_3.Retrieve(ai_Planta, 1, ai_Numero)
		
		/*
		Inserta Detalle Granel Orden de Proceso.
		*/
		IF Not(InsertaDetalleGranel()) THEN
			MessageBox("Atención","No fue posible insertar detalle Automático desde la Orden " + &
					"de Proceso.", Exclamation!, Ok!)
			lb_Retorno	=	False
		END IF
	END IF 
ELSE
	MessageBox("Atención","Número de Orden de Proceso No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean insertadetallegranel ();Long			ll_Fila, ll_FilaNea
Integer		li_Camara, li_PltCod, li_EspCod, li_Lote, li_TipoEnvase, li_CodEnvase, &
				li_CanBul
Decimal{2}	ld_KilPro, ld_KNetos
Boolean		lb_Retorno = True

FOR ll_Fila = 1 TO dw_3.RowCount()
	
	ll_FilaNea		=	dw_4.InsertRow(0)
	li_Camara		=	dw_3.Object.cama_codigo[ll_Fila]
	li_PltCod		=	dw_3.Object.lote_pltcod[ll_Fila]
	li_EspCod		=	dw_3.Object.lote_espcod[ll_Fila]
	li_Lote			=	dw_3.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_3.Object.enva_tipoen[ll_Fila]
	li_CodEnvase	=	dw_3.Object.enva_codigo[ll_Fila]
	li_CanBul		=	dw_3.Object.orpd_canbul[ll_Fila]
	
	SELECT	lotd_kilpro
		INTO	:ld_KilPro
		FROM	dba.spro_lotesfrutagrandeta
		WHERE	lote_pltcod	=	:li_PltCod
		AND	lote_espcod	=	:li_EspCod
		AND	lote_codigo	=	:li_Lote
		AND	enva_tipoen	=	:li_TipoEnvase
		AND	enva_codigo	=	:li_CodEnvase;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla LotesFrutaGranDeta")
		lb_Retorno	=	False
	ELSEIF sqlca.SQLCode <> 0 THEN
		lb_Retorno	=	False
	ELSE
		ld_KNetos		=	li_CanBul * ld_KilPro
		
		dw_4.SetItem(ll_FilaNea, "plde_codigo", Integer(istr_mant.argumento[1]))
		dw_4.SetItem(ll_FilaNea, "tpmv_codigo", Integer(istr_mant.argumento[2]))
		dw_4.SetItem(ll_FilaNea, "plde_coorde", Integer(istr_mant.argumento[12]))
		dw_4.SetItem(ll_FilaNea, "cama_codigo", li_Camara)
		dw_4.SetItem(ll_FilaNea, "lote_pltcod", li_PltCod)
		dw_4.SetItem(ll_FilaNea, "lote_espcod", li_EspCod)
		dw_4.SetItem(ll_FilaNea, "lote_codigo", li_Lote)
		dw_4.SetItem(ll_FilaNea, "enva_tipoen", li_TipoEnvase)
		dw_4.SetItem(ll_FilaNea, "enva_codigo", li_CodEnvase)
		dw_4.SetItem(ll_FilaNea, "mfgd_bulent", li_CanBul)
		dw_4.SetItem(ll_FilaNea, "mfgd_kgnent", ld_KNetos)
	END IF
	
NEXT

RETURN lb_Retorno
end function

public function boolean revisaenvases ();Long ll_Fila, ll_FilaBusc, ll_Filaarr,arr_envases[500,4], ll_sumBultos, ll_Row, ll_FilaUlt
Integer  li_TipoEnva, li_Envase, li_TipoEnvaSig, li_EnvaseSig, li_cont
String   ls_mensaje
Boolean  lb_ya_ingresado = False

ll_Fila = 1
ll_FilaArr = 1

Do While  ll_Fila<= dw_3.RowCount()
	
	ll_sumBultos = 0
	lb_ya_ingresado = False
	
   li_Tipoenva =	dw_3.Object.enva_tipoen[ll_Fila]
	li_envase	=	dw_3.Object.enva_codigo[ll_Fila]
		
	FOR ll_Row=1 To UpperBound(arr_envases)
		IF arr_envases[ll_row,1] = li_TipoEnva AND arr_envases[ll_row,2] = li_Envase THEN
			lb_ya_ingresado = TRUE
			ll_row = UpperBound(arr_envases)
		ELSE
			IF arr_envases[ll_row,1] = 0 THEN ll_row = UpperBound(arr_envases)
		END IF
	NEXT	
	
	IF lb_ya_ingresado = False THEN
		
		ll_SumBultos = dw_3.Object.mfgd_bulent[ll_Fila]
		
		FOR ll_FilaBusc = (ll_fila + 1) TO dw_3.RowCount()
			li_TipoEnvaSig =	dw_3.Object.enva_tipoen[ll_FilaBusc]
			li_EnvaseSig	=	dw_3.Object.enva_codigo[ll_FilaBusc]
			IF li_TipoEnva = li_TipoEnvaSig And li_Envase=li_EnvaseSig THEN
				ll_SumBultos = ll_SumBultos + dw_3.Object.mfgd_bulent[ll_FilaBusc]
			END IF	
		NEXT	
		
		arr_envases[ll_FilaArr,1] = li_TipoEnva
		arr_envases[ll_FilaArr,2] = li_Envase
		arr_envases[ll_FilaArr,3] = ll_SumBultos
		arr_envases[ll_FilaArr,4] = 0
		ll_FilaArr++
	END IF
	ll_Fila++
LOOP	

ll_Fila    = 1
ll_FilaUlt = ll_FilaArr

DO WHILE  ll_Fila<= dw_4.RowCount()
	
	ll_sumBultos = 0
	lb_ya_ingresado = False
	
   li_Tipoenva =	dw_4.Object.enva_tipoen[ll_Fila]
	li_envase	=	dw_4.Object.enva_codigo[ll_Fila]
		
	FOR ll_Row=1 To UpperBound(arr_envases)
		IF arr_envases[ll_Row,1] = li_TipoEnva AND arr_envases[ll_Row,2] = li_Envase THEN
			lb_ya_ingresado	=	TRUE
			ll_FilaArr			=	ll_Row
			ll_row				=	UpperBound(arr_envases)
		ELSE
			IF arr_envases[ll_Row,1] = 0 THEN ll_row	=	UpperBound(arr_envases)
		END IF
	NEXT	
	
	IF lb_ya_ingresado THEN
		
		IF arr_envases[ll_FilaArr,4] = 0 THEN
			ll_SumBultos = dw_4.Object.fgme_cantid[ll_Fila]
			
			FOR ll_FilaBusc = (ll_fila + 1) TO dw_4.RowCount()
				li_TipoEnvaSig	=	dw_4.Object.enva_tipoen[ll_FilaBusc]
				li_EnvaseSig	=	dw_4.Object.enva_codigo[ll_FilaBusc]
				IF li_TipoEnva = li_TipoEnvaSig And li_Envase=li_EnvaseSig THEN
					ll_SumBultos = ll_SumBultos + dw_4.Object.fgme_cantid[ll_FilaBusc]
				END IF	
			NEXT	
			
			arr_envases[ll_FilaArr,4] = ll_SumBultos
		END IF	
	ELSE
		arr_envases[ll_FilaUlt,1] = li_TipoEnva
		arr_envases[ll_FilaUlt,2] = li_Envase
		arr_envases[ll_FilaUlt,3] = 0
      arr_envases[ll_FilaUlt,4] = dw_4.Object.fgme_cantid[ll_Fila]
		ll_FilaUlt++
	END IF
	ll_Fila++
LOOP	

FOR ll_Fila=1 TO UpperBound(arr_envases)
	IF arr_envases[ll_Fila,3]<>arr_envases[ll_Fila,4] THEN
   	li_cont ++
	   ls_mensaje = 	ls_mensaje + "~nTipo Envase " + String(arr_envases[ll_Fila,1]) 
		ls_mensaje = 	ls_mensaje + "  y Envase " + String(arr_envases[ll_Fila,2]) 
		ls_mensaje = 	ls_mensaje + "  Bultos (" + String(arr_envases[ll_Fila,3])
		ls_mensaje = 	ls_mensaje + ") (" + String(arr_envases[ll_Fila,4]) + ")"
	   IF arr_envases[ll_Fila,1] = 0 THEN ll_Fila = UpperBound(arr_envases)	
   END IF		
NEXT

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "No Existe Concordancia de Bultos en :" + ls_mensaje + ".", StopSign!, Ok!)
	RETURN FALSE
END IF


RETURN TRUE

end function

public function boolean existedespacho (integer ai_planta, integer ai_tipomovto, long al_numero);
Long		ll_Numero
Boolean	lb_Retorno = True

SELECT	mfge_numero
	INTO	:ll_Numero
	FROM	dba.spro_movtofrutagranenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfge_numero	=	:al_Numero ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Despachos Fruta Granel")
	
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

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ld_Fecha


IF as_Columna <> "prod_codigo" AND &
	(dw_2.Object.prod_codigo[1] = 0 OR IsNull(dw_2.Object.prod_codigo[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "moti_codigo" AND &
	(dw_2.Object.moti_codigo[1] = 0 OR IsNull(dw_2.Object.moti_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "defg_docrel" AND &
	(dw_2.Object.defg_docrel[1] = 0 OR IsNull(dw_2.Object.defg_docrel[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "mfge_fecmov" AND &
	(dw_2.Object.mfge_fecmov[1] = ld_Fecha OR IsNull(dw_2.Object.mfge_fecmov[1])) THEN
	lb_Estado = False
END IF

tab_1.tp_1.Enabled	=	lb_Estado
tab_1.tp_2.Enabled	=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado
end subroutine

event open;/* 
Argumentos
----------
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Tipo de Movimiento
istr_Mant.Argumento[3]	=	Número de Despacho a Proceso
istr_Mant.Argumento[4]	=	Tipo Dcto Relacionado
*/

dw_4	=	tab_1.tp_1.dw_detalle
dw_5	=	tab_1.tp_2.dw_envases

dw_2.GetChild("moti_codigo", idwc_Motivo)
idwc_Motivo.SetTransObject(sqlca)

IF idwc_Motivo.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Motivos de Despacho")
	idwc_Motivo.InsertRow(0)
ELSE
	idwc_Motivo.SetSort("moti_nombre A")
	idwc_Motivo.Sort()
END IF

dw_2.GetChild("plde_coorde", idwc_Packing)
idwc_Packing.SetTransObject(sqlca)

IF idwc_Packing.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Packing para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	idwc_Packing.InsertRow(0)
ELSE
	idwc_packing.SetFilter("plde_tipopl in(1,3)")
	idwc_packing.Filter()
	idwc_Packing.SetSort("plde_nombre A")
	idwc_Packing.Sort()
END IF

dw_3.GetChild("enva_codigo", idwc_Envases)
idwc_Envases.SetTransObject(sqlca)

IF idwc_Envases.Retrieve(0) = 0 THEN
	MessageBox("Atención", "Falta Registrar los Envases")
	idwc_Envases.InsertRow(0)
END IF

dw_3.GetChild("cama_codigo", idwc_Camara)
idwc_Camara.SetTransObject(sqlca)

IF idwc_Camara.Retrieve(0) = 0 THEN
	MessageBox("Atención", "Falta Registrar las Camaras")
	idwc_Camara.InsertRow(0)
END IF

dw_4.GetChild("cama_codigo", idwc_Camara)
idwc_Camara.SetTransObject(sqlca)

IF idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	idwc_Camara.InsertRow(0)
ELSE
	idwc_Camara.SetSort("cama_nombre A")
	idwc_Camara.Sort()
END IF

dw_4.GetChild("lote_pltcod", idwc_PltaLote)
idwc_PltaLote.SetTransObject(sqlca)
idwc_PltaLote.Retrieve()

dw_4.GetChild("lote_espcod", idwc_Especie)
idwc_Especie.SetTransObject(sqlca)
idwc_Especie.Retrieve()

dw_5.GetChild("enva_tipoen", idwc_TipoEnvase)
idwc_TipoEnvase.SetTransObject(sqlca)

IF idwc_TipoEnvase.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Envases ")
	idwc_TipoEnvase.InsertRow(0)
ELSE
	idwc_TipoEnvase.SetSort("tien_nombre A")
	idwc_TipoEnvase.Sort()
END IF

dw_5.GetChild("cama_codigo", idwc_Camara)
idwc_Camara.SetTransObject(sqlca)

IF idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	idwc_Camara.InsertRow(0)
ELSE
	idwc_Camara.SetSort("cama_nombre A")
	idwc_Camara.Sort()
END IF

Call Super::Open

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	Message.StringParm
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	"4"

dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)

dw_4.Modify("datawindow.message.title='Error '+ is_titulo")
dw_4.SetRowFocusIndicator(Hand!)
dw_4.Modify("DataWindow.Footer.Height = 88")

dw_5.Modify("datawindow.message.title='Error '+ is_titulo")
dw_5.SetRowFocusIndicator(Hand!)
dw_5.Modify("DataWindow.Footer.Height = 88")

iuo_Packing			=	Create uo_plantadesp
iuo_MotiDespacho	=	Create uo_motivodespacho

end event

event ue_borra_detalle();call super::ue_borra_detalle;SetPointer(HourGlass!)

ib_Borrar				=	True
Message.DoubleParm	=	0
istr_mant.Borra		=	True
istr_mant.Agrega		=	False

w_main.SetMicroHelp("Validando la eliminación de detalle...")

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF tab_1.SelectedTab = 1 THEN
	istr_Mant.dw	=	dw_4
	
	OpenWithParm(iw_mantencion_1, istr_Mant)
	
	istr_Mant	=	Message.PowerObjectParm
	
	IF istr_mant.Respuesta = 1 THEN
		IF dw_4.DeleteRow(0) = 1 THEN
			ib_Borrar	=	False
			
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_Borrar	=	False
			
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_4.RowCount() = 0 THEN 
			HabilitaEncab(True)
			
			pb_eli_det.Enabled			=	False
			dw_2.Object.mfge_totbul[1]	=	0
			dw_2.Object.mfge_tpneto[1]	=	0
		ELSE
			dw_2.Object.mfge_totbul[1]	=	Round(dw_4.Object.total_bultos[1], 0)
			dw_2.Object.mfge_tpneto[1]	=	Round(dw_4.Object.total_kilos[1], 3)
		END IF
	END IF
ELSE
	istr_Mant.dw	=	dw_5
	
	OpenWithParm(iw_mantencion_2, istr_Mant)
	
	istr_Mant	=	Message.PowerObjectParm
	
	IF istr_mant.Respuesta = 1 THEN
		IF dw_5.DeleteRow(0) = 1 THEN
			ib_Borrar	=	False
			
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_Borrar	=	False
			
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_5.RowCount() = 0 THEN 
			HabilitaEncab(True)
			pb_eli_det.Enabled	=	False
		END IF
	END IF
END IF

istr_mant.Borra	 = False
end event

event ue_nuevo_detalle();call super::ue_nuevo_detalle;Integer li_row
Long  ll_suma

istr_mant.Borra	=	False
istr_mant.Agrega	=	True
	
IF tab_1.SelectedTab = 1 THEN
	istr_mant.dw	=	dw_4
	
	OpenWithParm(iw_mantencion_1, istr_mant)

	IF istr_mant.respuesta = 1 THEN
		IF dw_4.RowCount() > 0 THEN
			dw_2.Object.mfge_totbul[1]	=	Round(dw_4.Object.total_bultos[1], 0)
			dw_2.Object.mfge_tpneto[1]	=	Round(dw_4.Object.total_kilos[1], 3)
			pb_eli_det.Enabled			=	True
		ELSE
			dw_2.Object.mfge_totbul[1]	=	0
			dw_2.Object.mfge_tpneto[1]	=	0
			pb_eli_det.Enabled			=	False
		END IF
	END IF
ELSE
	istr_mant.dw	=	dw_5

	OpenWithParm(iw_mantencion_2, istr_mant)

	IF dw_5.RowCount() > 0 THEN
		pb_eli_det.Enabled			=	True
	ELSE
		pb_eli_det.Enabled			=	False
	END IF
END IF

IF dw_4.RowCount() > 0 AND dw_5.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_4.RowCount() > 0 AND dw_5.RowCount() > 0 AND Not pb_eliminar.Enabled THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
END IF

IF tab_1.SelectedTab = 1 THEN
	dw_4.SetRow(il_Fila)
	dw_4.SelectRow(il_Fila, True)
ELSE
	dw_5.SetRow(il_Fila)
	dw_5.SelectRow(il_Fila, True)
END IF
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)

	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
											Integer(istr_Mant.Argumento[2]), &
											Long(istr_Mant.Argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			IF dw_4.Retrieve(Integer(istr_mant.argumento[1]), &
									Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3])) = -1 OR &
				dw_1.Retrieve(Integer(istr_mant.argumento[1]), &
									Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3])) = -1 OR &
				dw_5.Retrieve(Integer(istr_mant.argumento[1]), &
									Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]), 2) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", &
												"No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	=	True
				pb_grabar.Enabled		=	True
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True

				HabilitaEncab(False)
				
				pb_eli_det.Enabled	=	True
				
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

on w_maed_movtofrutagranel_traspaso_plantas.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.dw_3
end on

on w_maed_movtofrutagranel_traspaso_plantas.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.dw_3)
end on

event ue_modifica_detalle();integer li_row
Long    ll_suma

istr_mant.Agrega	=	False
istr_mant.Borra	=	False

IF tab_1.SelectedTab = 1 THEN
	IF dw_4.RowCount() > 0 THEN
		istr_mant.dw	=	dw_4
		
		OpenWithParm(iw_mantencion_1, istr_mant)
			
		IF istr_mant.respuesta = 1 THEN
			dw_2.Object.mfge_totbul[1]	=	Round(dw_4.Object.total_bultos[1], 0)
			dw_2.Object.mfge_tpneto[1]	=	Round(dw_4.Object.total_kilos[1], 3)
		END IF
	END IF
ELSE
	IF dw_5.RowCount() > 0 THEN
		
		istr_mant.dw	=	dw_5
		
		OpenWithParm(iw_mantencion_2, istr_mant)
	END IF
END IF
end event

event ue_nuevo();call super::ue_nuevo;dw_4.Reset()
dw_5.Reset()

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	Integer(istr_Mant.Argumento[2])
dw_2.Object.mfge_fecmov[1]	=	Date(Today())
dw_2.Object.mfge_estmov[1] =  1
dw_2.Object.defg_tipdoc[1]	=	4
end event

event ue_antesguardar();call super::ue_antesguardar;Long		ll_Fila, ll_Numero
Integer	li_Secuencia, li_Planta, li_TipoMovto

//	Control de Envases
IF NOT revisaenvases() THEN
	Message.DoubleParm = -1
	RETURN
END IF	

IF dw_4.Object.total_bultos[1] <> dw_5.Object.total_envases[1] THEN
	MessageBox("Error de Consistencia", "Cantidad de Bultos Despachados debe " + &
					"corresponder con Cantidad de Envases.~r~r" + &
					"Corrija Detalle de Fruta o Detalle de Envases.")
					
	Message.DoubleParm = -1
ELSE
	ib_AutoCommit		=	sqlca.AutoCommit
	sqlca.AutoCommit	=	False
	
	li_Planta			=	dw_2.Object.plde_codigo[1]
	li_TipoMovto		=	dw_2.Object.tpmv_codigo[1]
	ll_Numero			=	dw_2.Object.mfge_numero[1]
	
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		UPDATE	dba.spro_movtofrutagranenca
			SET	mfge_numero = 0
			WHERE	1 = 2;
		
		SELECT	IsNull(Max(mfge_numero), 0) + 1
			INTO	:ll_Numero
			FROM	dba.spro_movtofrutagranenca
			WHERE	plde_codigo	=	:li_Planta
			AND	tpmv_codigo	=	:li_TipoMovto ;
	
		dw_2.Object.mfge_numero[1]	=	ll_Numero
	
		ll_Fila	=	dw_1.InsertRow(0)
		
		dw_1.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_1.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.meen_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
		dw_1.Object.plde_coorde[ll_Fila]	=	dw_2.Object.plde_coorde[1]
		dw_1.Object.prod_codigo[ll_Fila]	=	dw_2.Object.prod_codigo[1]
		dw_1.Object.meen_fecmov[ll_Fila]	=	dw_2.Object.mfge_fecmov[1]
		dw_1.Object.meen_modulo[ll_Fila]	=	1
		dw_1.Object.tpmv_codrec[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
	END IF
	
	istr_mant.Argumento[3]	=	String(dw_2.Object.mfge_numero[1])
	
	SELECT	IsNull(Max(mfgd_secuen), 0) + 1
		INTO	:li_Secuencia
		FROM	dba.spro_movtofrutagrandeta
		WHERE	plde_codigo	=	:li_Planta
		AND	tpmv_codigo	=	:li_TipoMovto
		AND	mfge_numero	=	:ll_Numero ;
		
	FOR ll_Fila = 1 TO dw_4.RowCount()
		IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
			dw_4.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
			dw_4.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
			dw_4.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
			dw_4.Object.mfgd_secuen[ll_Fila]	=	li_Secuencia
			
			li_Secuencia ++
		END IF
	NEXT
	
	FOR ll_Fila = 1 TO dw_5.RowCount()
		IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
			dw_5.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
			dw_5.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
			dw_5.Object.meen_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
			
			li_Secuencia ++
		END IF
	NEXT
END IF
end event

event ue_borrar();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar		=	True
ib_AutoCommit	=	sqlca.AutoCommit

w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_4.RowCount() > 0 THEN
	dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
END IF

IF dw_5.RowCount() > 0 THEN
	dw_5.RowsMove(1, dw_5.RowCount(), Primary!, dw_5, 1, Delete!)
END IF

IF dw_2.DeleteRow(0) = 1 AND dw_1.DeleteRow(0) = 1 THEN
		ib_Borrar	=	False
		
		w_main.SetMicroHelp("Borrando Registro...")
		
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_Borrar	=	False
	
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

event ue_seleccion();call super::ue_seleccion;Str_Busqueda	lstr_busq
Date				ld_FechaInicio

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[1]
lstr_Busq.Argum[2]	=	istr_Mant.Argumento[2]
lstr_Busq.Argum[3]	=	'1'
lstr_Busq.Argum[4]	=	String(ld_FechaInicio)

OpenWithParm(w_busc_spro_movtofrutagranenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	istr_Mant.Argumento[1]	=	lstr_Busq.Argum[1]
	istr_Mant.Argumento[2]	=	lstr_Busq.Argum[2]
	istr_Mant.Argumento[3]	=	lstr_Busq.Argum[3]
	
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

tab_1.Width							=	This.WorkSpaceWidth() - 374
maximo 								=	tab_1.width

dw_2.x								=	37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y								=	37

tab_1.x								=	37 + Round((maximo - tab_1.width) / 2, 0)
tab_1.y								=	64 + dw_2.Height
tab_1.height						=	This.WorkSpaceHeight() - tab_1.y - 41

tab_1.tp_1.dw_detalle.x			= 27
tab_1.tp_1.dw_detalle.y			= 36
tab_1.tp_1.dw_detalle.height	= tab_1.height - 180
tab_1.tp_1.dw_detalle.width	= tab_1.width - 92

tab_1.tp_2.dw_envases.x			= 27
tab_1.tp_2.dw_envases.y			= 36
tab_1.tp_2.dw_envases.height	= tab_1.height - 180
tab_1.tp_2.dw_envases.width	= tab_1.width - 92


li_posic_x							= This.WorkSpaceWidth() - 250
li_posic_y							= 350

IF pb_buscar.Visible THEN
	pb_buscar.x						= li_posic_x
	pb_buscar.y						= li_posic_y
	pb_buscar.width				= 156
	pb_buscar.height				= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x						= li_posic_x
	pb_nuevo.y						= li_posic_y
	pb_nuevo.width					= 156
	pb_nuevo.height				= 133
	li_visible ++
	li_posic_y += 180
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x					= li_posic_x
	pb_eliminar.y					= li_posic_y
	pb_eliminar.width				= 156
	pb_eliminar.height			= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x						= li_posic_x
	pb_grabar.y						= li_posic_y
	pb_grabar.width				= 156
	pb_grabar.height				= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x					= li_posic_x
	pb_imprimir.y					= li_posic_y
	pb_imprimir.width				= 156
	pb_imprimir.height			= 133
	li_visible ++
	li_posic_y += 180
END IF

IF pb_salir.Visible THEN
	pb_salir.x						= li_posic_x
	pb_salir.y						= li_posic_y
	pb_salir.width					= 156
	pb_salir.height				= 133
	li_visible ++
	li_posic_y += 180
END IF

pb_ins_det.x						= li_posic_x
pb_ins_det.y						= 1300
pb_ins_det.width					= 156
pb_ins_det.height					= 133

pb_eli_det.x						= li_posic_x
pb_eli_det.y						= pb_ins_det.y + 180
pb_eli_det.width					= 156
pb_eli_det.height					= 133
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutagranel_traspaso_plantas
boolean visible = false
integer x = 224
integer y = 1260
integer width = 795
integer height = 412
boolean enabled = false
string title = ""
string dataobject = "dw_mant_movtoenvaenca"
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutagranel_traspaso_plantas
integer x = 41
integer y = 40
integer width = 2747
integer height = 948
integer taborder = 10
string dataobject = "dw_mant_movtofrutagranel_traspaso_planta"
end type

event dw_2::itemchanged;String	ls_Nula
String	ls_Columna

ls_Columna	=	dwo.Name
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
		
	CASE "mfge_numero"
		IF NOT ExisteDespacho(gstr_ParamPlanta.CodigoPlanta, Integer(istr_mant.argumento[2]), Integer(data)) THEN
			This.SetItem(1,"mfge_numero", Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF


	CASE "plde_coorde"
		IF Not iuo_Packing.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			RETURN 1
		ELSE
			istr_mant.argumento[12]	=	Data
		END IF
		
	CASE "moti_codigo"
		IF Not iuo_MotiDespacho.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF

	CASE "defg_docrel"
		IF Not Existedocproceso(gstr_ParamPlanta.CodigoPlanta,Integer(Data)) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			RETURN 1
		ELSE
			dw_2.Setitem(1, "defg_docrel", Integer(istr_mant.argumento[5]))
			dw_2.Setitem(1, "prod_codigo", Long(istr_mant.argumento[6]))
			dw_2.Setitem(1, "prod_nombre", istr_mant.argumento[7])
			dw_2.Setitem(1, "espe_codigo", Integer(istr_mant.argumento[8]))
		END IF
	
END CHOOSE

HabilitaIngreso(ls_Columna)

end event

event dw_2::doubleclicked;//
end event

event dw_2::buttonclicked;call super::buttonclicked;Str_Busqueda	lstr_busq
Long ll_null

Setnull(ll_null)

CHOOSE CASE dwo.Name
	CASE "b_bucaordenproceso"
		
		lstr_busq.argum[1]	=	istr_mant.argumento[1] //Planta
		lstr_busq.argum[2]	=	"1"                    //Estado
		lstr_busq.argum[3]	=	"4"                    //Tipo orden  
		
		OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)
		lstr_busq	=	Message.PowerObjectParm

		IF lstr_busq.argum[2] <> "" THEN
			IF Not Existedocproceso(gstr_ParamPlanta.CodigoPlanta,long(lstr_busq.argum[6])) THEN
				This.SetItem(1, 'defg_docrel', ll_Null)
				RETURN 1
			ELSE
				dw_2.Setitem(1, "defg_docrel", long(istr_mant.argumento[5]))
				dw_2.Setitem(1, "prod_codigo", Long(istr_mant.argumento[6]))
				dw_2.Setitem(1, "prod_nombre", istr_mant.argumento[7])
				dw_2.Setitem(1, "espe_codigo", Integer(istr_mant.argumento[8]))

				Habilitaencab(False)
				Habilitaingreso("defg_docrel")

			END IF			
		END IF
		
END CHOOSE		
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutagranel_traspaso_plantas
integer x = 2939
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(TRUE)
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutagranel_traspaso_plantas
integer x = 2939
integer taborder = 70
end type

event pb_eliminar::clicked;call super::clicked;str_Mant		lstr_Mant

lstr_Mant.Argumento[1]	=	"1"
lstr_Mant.Argumento[2]	=	istr_Mant.Argumento[1]
lstr_Mant.Argumento[3]	=	istr_Mant.Argumento[2]
lstr_Mant.Argumento[4]	=	istr_Mant.Argumento[3]
lstr_Mant.Argumento[5]	=	"M"

OpenWithParm(w_mant_bitacorasitua, lstr_Mant)
end event

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutagranel_traspaso_plantas
integer x = 2939
integer y = 632
integer taborder = 80
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutagranel_traspaso_plantas
integer x = 2939
integer taborder = 90
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutagranel_traspaso_plantas
integer x = 2939
integer taborder = 100
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutagranel_traspaso_plantas
integer x = 2939
integer y = 1504
integer taborder = 30
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutagranel_traspaso_plantas
integer x = 2939
integer y = 1680
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutagranel_traspaso_plantas
integer x = 2939
integer taborder = 50
end type

type tab_1 from tab within w_maed_movtofrutagranel_traspaso_plantas
event create ( )
event destroy ( )
integer x = 37
integer y = 1008
integer width = 2747
integer height = 848
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.Control[]={this.tp_1,&
this.tp_2}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
end on

event selectionchanged;IF NewIndex = 1 THEN
	IF dw_4.RowCount() > 0 THEN
		pb_eli_det.Enabled	=	True
		il_Fila 					=	1
		
		dw_4.SelectRow(0,False)
		dw_5.SelectRow(0,False)
		dw_4.SetRow(il_Fila)
		dw_4.SelectRow(il_Fila, True)
	ELSE
		pb_eli_det.Enabled	=	False
	END IF
ELSE
	IF dw_5.RowCount() > 0 THEN
		pb_eli_det.Enabled	=	True
		il_Fila 					=	1
		
		dw_4.SelectRow(0,False)
		dw_5.SelectRow(0,False)
		dw_5.SetRow(il_Fila)
		dw_5.SelectRow(il_Fila, True)
	ELSE
		pb_eli_det.Enabled	=	False
	END IF
END IF
end event

type tp_1 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 2711
integer height = 720
boolean enabled = false
long backcolor = 12632256
string text = "Detalle de Fruta"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Table!"
long picturemaskcolor = 553648127
dw_detalle dw_detalle
end type

on tp_1.create
this.dw_detalle=create dw_detalle
this.Control[]={this.dw_detalle}
end on

on tp_1.destroy
destroy(this.dw_detalle)
end on

type dw_detalle from uo_dw within tp_1
integer x = 18
integer y = 36
integer width = 2994
integer height = 668
integer taborder = 11
string dataobject = "dw_mues_movtofrutagraneldeta_despacho"
boolean hscrollbar = true
boolean hsplitscroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_proceso.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutagranel_despacho.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_despacho.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event getfocus;call super::getfocus;IF il_fila > 0 THEN This.SelectRow(il_fila, True)

RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

type tp_2 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 2711
integer height = 720
boolean enabled = false
long backcolor = 12632256
string text = "Detalle de Envases       "
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "ArrangeTables!"
long picturemaskcolor = 553648127
dw_envases dw_envases
end type

on tp_2.create
this.dw_envases=create dw_envases
this.Control[]={this.dw_envases}
end on

on tp_2.destroy
destroy(this.dw_envases)
end on

type dw_envases from uo_dw within tp_2
integer x = 37
integer y = 36
integer width = 2994
integer height = 780
integer taborder = 10
string dataobject = "dw_mues_movtoenvadeta"
boolean hscrollbar = true
boolean hsplitscroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_proceso.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutagranel_despacho.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_despacho.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event getfocus;call super::getfocus;IF il_fila > 0 THEN This.SelectRow(il_fila, True)

RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

type dw_3 from datawindow within w_maed_movtofrutagranel_traspaso_plantas
boolean visible = false
integer x = 1147
integer y = 1316
integer width = 1536
integer height = 432
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_ordenprocdeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

