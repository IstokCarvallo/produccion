$PBExportHeader$w_maed_movtoserviadienca_planta.srw
forward
global type w_maed_movtoserviadienca_planta from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_movtoserviadienca_planta
end type
type dw_4 from datawindow within w_maed_movtoserviadienca_planta
end type
end forward

global type w_maed_movtoserviadienca_planta from w_mant_encab_deta_csd
integer width = 4832
integer height = 2264
string title = "MOVIMIENTOS SERVICIOS ADICIONALES"
string menuname = ""
event ue_imprimir ( )
dw_3 dw_3
dw_4 dw_4
end type
global w_maed_movtoserviadienca_planta w_maed_movtoserviadienca_planta

type variables
DataWindowChild	idwc_planta, idwc_cliente, idwc_servicio, idwc_detalle
Integer	ii_estado, il_fila1

Date							id_Inicio
end variables

forward prototypes
public subroutine habilitaingreso (string columna)
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean noexistecliente (integer ai_codigo)
public function boolean noexisteservicios (string columna, integer ai_codigo)
public function boolean existeregistro (integer ai_cliente, integer ai_planta, long al_numero)
public function long existefolio (integer ai_cliente, integer ai_planta)
public function boolean existe_planta (integer ai_codigo)
public subroutine recupera_caracteristicas (integer ai_cliente, integer ai_planta, long ai_pallet)
public function boolean duplicado (long al_numero)
public function boolean noexistepallet (long al_numero)
public subroutine buscapallet ()
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MOVIMIENTOS SERVICIOS ADICIONALES"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_movtoservimovtos"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[2]), integer(istr_mant.argumento[1]),&
								Long(istr_mant.argumento[3]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

public subroutine habilitaingreso (string columna);Boolean	lb_estado = True

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
		IsNull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0 OR &
		IsNull(dw_2.Object.sere_codigo[1]) OR dw_2.Object.sere_codigo[1] = 0 OR &
		IsNull(dw_2.Object.serd_codigo[1]) OR dw_2.Object.serd_codigo[1] = 0 THEN
		lb_estado = False
	END IF
END IF

pb_ins_det.Enabled = lb_estado
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect		=	0
	dw_2.Object.plde_codigo.Protect		=	0
	dw_2.Object.mose_numero.Protect	=	0
	dw_2.Object.sere_codigo.Protect		=	0
	dw_2.Object.serd_codigo.Protect		=	0	
	dw_2.Object.mose_fecmov.Protect	=	0
	dw_2.Object.mose_hormov.Protect	=	0
	
	dw_2.Object.mose_numero.Color	= 0
	dw_2.Object.clie_codigo.Color 		= 0
	dw_2.Object.plde_codigo.Color 	= 0
	dw_2.Object.sere_codigo.Color 	= 0
	dw_2.Object.serd_codigo.Color 	= 0
	dw_2.Object.mose_fecmov.Color 	= 0
	dw_2.Object.mose_hormov.Color 	= 0
	
	dw_2.Object.mose_numero.BackGround.Color	= Rgb(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color 		= Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.sere_codigo.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.serd_codigo.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.mose_fecmov.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.mose_hormov.BackGround.Color 	= Rgb(255,255,255)
	
	dw_2.SetColumn("mose_numero")
	dw_2.SetFocus()
ELSE
	dw_2.Object.mose_numero.Protect	=	1
	dw_2.Object.clie_codigo.Protect		=	1
	dw_2.Object.plde_codigo.Protect		=	1
	dw_2.Object.sere_codigo.Protect		=	1
	dw_2.Object.serd_codigo.Protect		=	1
	dw_2.Object.mose_fecmov.Protect	=	1
	dw_2.Object.mose_hormov.Protect	=	1
	
	dw_2.Object.mose_numero.Color	= Rgb(255,255,255)
	dw_2.Object.clie_codigo.Color 		= Rgb(255,255,255)
	dw_2.Object.plde_codigo.Color 	= Rgb(255,255,255)
	dw_2.Object.sere_codigo.Color 	= Rgb(255,255,255)
	dw_2.Object.serd_codigo.Color 	= Rgb(255,255,255)
	dw_2.Object.mose_fecmov.Color 	= Rgb(255,255,255)
	dw_2.Object.mose_hormov.Color 	= Rgb(255,255,255)
	
	dw_2.Object.mose_numero.BackGround.Color	= 553648147
	dw_2.Object.clie_codigo.BackGround.Color 		= 553648147
	dw_2.Object.plde_codigo.BackGround.Color 	= 553648147
	dw_2.Object.sere_codigo.BackGround.Color 	= 553648147
	dw_2.Object.serd_codigo.BackGround.Color 	= 553648147
	dw_2.Object.mose_fecmov.BackGround.Color 	= 553648147
	dw_2.Object.mose_hormov.BackGround.Color 	= 553648147
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
      IF dw_3.Update(True, False) = 1 THEN
			 IF dw_2.Update(True, False) = 1 THEN
				Commit;
				
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
					ELSE
						lb_Retorno	=	True
						
						dw_1.ResetUpdate()
						dw_3.ResetUpdate()
						dw_2.ResetUpdate()
					END IF
			 ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			 END IF
	   ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
		END IF
   ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
					
					RollBack;
				ELSE
					lb_Retorno	=	True
					
					dw_3.ResetUpdate()
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
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
END IF
dw_3.Reset()
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre  
   FROM	dbo.clientesprod  
   WHERE	clie_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN True
END IF

RETURN False

end function

public function boolean noexisteservicios (string columna, integer ai_codigo);String	ls_nombre
Integer	li_planta, cont

li_planta = Integer(istr_mant.argumento[1])

IF columna = 'sere_codigo' THEN
  SELECT count()  
    INTO :cont
    FROM dbo.serviplantaenca  
   WHERE sere_codigo = :ai_codigo ;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura Tabla Servicios encabezado")
		RETURN True
	ELSEIF cont = 0 THEN
		MessageBox("Error", "Servicio no Existe. Ingrese otro.")
		RETURN True
	ELSE
		RETURN False
	END IF
ELSE
	 SELECT count()  
    INTO :cont
    FROM dbo.serviplantadeta 
   WHERE sere_codigo = :ai_codigo ;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura Tabla Servicios detalle")
		RETURN True
	ELSEIF cont = 0 THEN
		MessageBox("Error", "Servicio Detalle no Existe. Ingrese otro.")
		RETURN True
	ELSE
		RETURN False
	END IF
END IF	




end function

public function boolean existeregistro (integer ai_cliente, integer ai_planta, long al_numero);Integer	li_cont

SELECT	count()
	INTO	:li_cont  
   FROM	dbo.movtoserviadienca  
   WHERE	clie_codigo =	:ai_cliente
	AND	plde_codigo =  :ai_planta
	AND	mose_numero =	:al_numero;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF li_cont = 0 THEN
	Return False
ELSE
	
	Return True
END IF

RETURN False

end function

public function long existefolio (integer ai_cliente, integer ai_planta);Long	ll_cont

SELECT	count()
	INTO	:ll_cont  
   FROM	dbo.movtoserviadienca  
   WHERE	clie_codigo =	:ai_cliente
	AND	plde_codigo =  :ai_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN 0
ELSEIF ll_cont = 0 THEN
	ll_cont = 1
ELSE
	ll_cont = ll_cont + 1
END IF

RETURN ll_cont

end function

public function boolean existe_planta (integer ai_codigo);Integer	ll_cont

SELECT	count()
	INTO	:ll_cont
   FROM	dbo.plantadesp 
   WHERE	plde_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Planta Producción")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Error", "Planta no Existe. Ingrese otra.")
	RETURN True
END IF

RETURN False

end function

public subroutine recupera_caracteristicas (integer ai_cliente, integer ai_planta, long ai_pallet);Integer	li_cont, li_fila, li_fila1, ll_fil, ll_new

SELECT	count()
	INTO	:li_cont  
   FROM	dbo.movtoserviadicaracteristicas  
   WHERE	clie_codigo =	:ai_cliente
	AND	plde_codigo =  :ai_planta
	AND	paen_numero =	:ai_pallet;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura movtoserviadicaracteristicas")
	RETURN
ELSEIF li_cont = 0 THEN
	li_fila = dw_3.Retrieve(ai_cliente,ai_planta,ai_pallet)
	
	IF li_fila = 0 THEN
		li_fila1 = dw_4.Retrieve(ai_cliente,ai_planta,ai_pallet)
	END IF	
	
	IF li_fila1 > 0 THEN
		FOR ll_fil = 1 TO dw_4.RowCount()
			ll_new = dw_3.InsertRow(0)
			
			dw_3.Object.plde_codigo[ll_new] = dw_1.Object.plde_codigo[il_fila]
			dw_3.Object.clie_codigo[ll_new] = dw_1.Object.clie_codigo[il_fila] 
			dw_3.Object.sere_codigo[ll_new] = dw_1.Object.sere_codigo[il_fila] 
			dw_3.Object.serd_codigo[ll_new] = dw_1.Object.serd_codigo[il_fila]  
			dw_3.Object.paen_numero[ll_new] = dw_1.Object.paen_numero[il_fila]  
			dw_3.Object.prod_nombre[ll_new] = dw_4.Object.prod_nombre[ll_fil]  
			dw_3.Object.prod_codigo[ll_new] = dw_4.Object.prod_codigo[ll_fil]  
			dw_3.Object.espe_codigo[ll_new] = dw_4.Object.espe_codigo[ll_fil]   
			dw_3.Object.vari_codigo[ll_new] = dw_4.Object.vari_codigo[ll_fil] 
			dw_3.Object.vari_nombre[ll_new] = dw_4.Object.vari_nombre[ll_fil]
			dw_3.Object.emba_codigo[ll_new] = dw_4.Object.emba_codigo[ll_fil]   
			dw_3.Object.cond_codigo[ll_new] = dw_4.Object.cond_codigo[ll_fil] 
			dw_3.Object.cond_nombre[ll_new] = dw_4.Object.cond_nombre[ll_fil] 
			dw_3.Object.etiq_codigo[ll_new] = dw_4.Object.etiq_codigo[ll_fil]   
			dw_3.Object.etiq_nombre[ll_new] = dw_4.Object.etiq_nombre[ll_fil]   
			dw_3.Object.pafr_calibr[ll_new] = dw_4.Object.pafr_calibr[ll_fil]   
			dw_3.Object.pafr_copack[ll_new] = dw_4.Object.pafr_copack[ll_fil]   
			dw_3.Object.plde_nombre[ll_new] = dw_4.Object.plde_nombre[ll_fil]   
			dw_3.Object.cate_codigo[ll_new] = dw_4.Object.cate_codigo[ll_fil]   
			dw_3.Object.cate_nombre[ll_new] = dw_4.Object.cate_nombre[ll_fil]   
			dw_3.Object.pafr_ccajas[ll_new] = dw_4.Object.pafr_ccajas[ll_fil]   
		NEXT
		dw_4.Reset()
	END IF	
	Return
ELSE
	
	li_fila = dw_3.Retrieve(ai_cliente,ai_planta,ai_pallet)
	Return
END IF

RETURN

end subroutine

public function boolean duplicado (long al_numero);Long		ll_fila
Integer	li_cliente, li_planta, li_cantid

ll_fila	=	dw_1.Find("clie_codigo = " + istr_mant.Argumento[2] + &
						" AND plde_codigo = " + istr_mant.Argumento[1] + &
						" AND paen_numero = " + String(al_Numero) , 1, dw_1.RowCount())
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Pallet ya fue incluido en Detalle", Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

public function boolean noexistepallet (long al_numero);String	ls_nomvar, ls_embala
Integer	li_catego, li_cliente, li_ContCalidad, li_tipopa, li_cajas, li_inspec, &
			li_Destino, li_Especie, li_Variedad, li_planta, li_condicion, li_estadofumi, li_respuesta
Long		ll_fumigacion, ll_fila, ll_fil, ll_new
Date		ld_fecfumi

li_cliente	= 	Integer(istr_mant.argumento[2])
li_planta	=	Integer(istr_mant.argumento[1])

SELECT	pae.paen_tipopa, var.vari_nombre, pae.emba_codigo, pae.cate_codigo,
			pae.paen_concal, pae.paen_ccajas, pae.paen_inspec, pae.dest_codigo,
			pae.espe_codigo, pae.vari_codigo, pae.paen_estado, pae.cond_codigo,
			pae.fumi_numero, pae.fumi_fecfum
	INTO	:li_tipopa, :ls_nomvar, :ls_embala, :li_catego, :li_ContCalidad, :li_cajas,
			:li_inspec, :li_Destino, :li_Especie, :li_Variedad, :ii_estado, :li_condicion,
			:ll_fumigacion,:ld_fecfumi
	FROM	dbo.palletencab as pae, dbo.variedades as var
	WHERE pae.clie_codigo	= 	:li_cliente
	AND	pae.paen_numero	= 	:al_numero
	AND	pae.plde_codigo	=	:li_planta
	AND	var.espe_codigo	= 	pae.espe_codigo
	AND	var.vari_codigo	= 	pae.vari_codigo
	AND   exists(select *
	from dbo.palletfruta as paf
	where paf.clie_codigo=pae.clie_codigo
	and   paf.plde_codigo=pae.plde_codigo
	and   paf.paen_numero=pae.paen_numero);

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Pallet no ha sido creado, Ingrese otro Código.", &
			Exclamation!, OK!)
	RETURN True
END IF

IF ii_estado = 2 THEN
	MessageBox("Atención", "Pallet ya fue Despachado desde Planta Despachadora.", &
			Exclamation!, OK!)
	RETURN True
ELSEIF ii_estado = 3 THEN
	MessageBox("Atención", "Pallet fue Repalletizado en Planta Despachadora.", &
			Exclamation!, OK!)
	RETURN True
END IF

dw_3.SetTransObject(Sqlca)

//ll_fila = dw_4.Retrieve(li_cliente,li_planta,al_numero)
//
//IF ll_fila > 0 THEN
//	FOR ll_fil = 1 TO ll_fila
//		ll_new = dw_2.InsertRow(0)
//		
//		dw_3.Object.plde_codigo[ll_new] = dw_1.Object.plde_codigo[il_fila]
//		dw_3.Object.clie_codigo[ll_new] = dw_1.Object.clie_codigo[il_fila] 
//		dw_3.Object.sere_codigo[ll_new] = dw_1.Object.sere_codigo[il_fila] 
//		dw_3.Object.serd_codigo[ll_new] = dw_1.Object.serd_codigo[il_fila]  
//		dw_3.Object.paen_numero[ll_new] = al_numero  
//		dw_3.Object.prod_nombre[ll_new] = dw_4.Object.prod_nombre[ll_fil]  
//		dw_3.Object.prod_codigo[ll_new] = dw_4.Object.prod_codigo[ll_fil]  
//		dw_3.Object.espe_codigo[ll_new] = dw_4.Object.espe_codigo[ll_fil]   
//		dw_3.Object.vari_codigo[ll_new] = dw_4.Object.vari_codigo[ll_fil] 
//		dw_3.Object.vari_nombre[ll_new] = dw_4.Object.vari_nombre[ll_fil]
//		dw_3.Object.emba_codigo[ll_new] = dw_4.Object.emba_codigo[ll_fil]   
//		dw_3.Object.cond_codigo[ll_new] = dw_4.Object.cond_codigo[ll_fil] 
//		dw_3.Object.cond_nombre[ll_new] = dw_4.Object.cond_nombre[ll_fil] 
//		dw_3.Object.etiq_codigo[ll_new] = dw_4.Object.etiq_codigo[ll_fil]   
//		dw_3.Object.etiq_nombre[ll_new] = dw_4.Object.etiq_nombre[ll_fil]   
//		dw_3.Object.pafr_calibr[ll_new] = dw_4.Object.pafr_calibr[ll_fil]   
//		dw_3.Object.pafr_copack[ll_new] = dw_4.Object.pafr_copack[ll_fil]   
//		dw_3.Object.plde_nombre[ll_new] = dw_4.Object.plde_nombre[ll_fil]   
//		dw_3.Object.cate_codigo[ll_new] = dw_4.Object.cate_codigo[ll_fil]   
//		dw_3.Object.cate_nombre[ll_new] = dw_4.Object.cate_nombre[ll_fil]   
//		dw_3.Object.pafr_ccajas[ll_new] = dw_4.Object.pafr_ccajas[ll_fil]   
//	NEXT
//END IF
dw_1.SetItem(il_fila, "paen_numero", al_numero)
dw_1.SetItem(il_fila, "espe_codigo", li_Especie)
//dw_1.SetItem(il_fila, "vari_codigo", li_Variedad)
dw_1.SetItem(il_fila, "vari_nombre", ls_nomvar)
//dw_1.SetItem(il_fila, "paen_tipopa", li_tipopa)
//dw_1.SetItem(il_fila, "emba_codigo", ls_embala)
//dw_1.SetItem(il_fila, "cate_codigo", li_catego)
dw_1.SetItem(il_fila, "paen_ccajas", li_cajas)
dw_1.SetColumn("mosd_observ")
dw_1.SetFocus()

RETURN False
end function

public subroutine buscapallet ();dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 5")

istr_busq.argum[2]	=	""
istr_busq.argum[1]	=	String(dw_1.Object.clie_codigo[il_fila])
istr_busq.argum[12]	=	String(dw_1.Object.plde_codigo[il_fila])
istr_busq.argum[3]	=	"1"

OpenWithParm(w_busc_palletencab_busc, istr_busq)

istr_busq	       	=	Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	IF Duplicado(Long(istr_busq.Argum[2])) THEN
		dw_1.SetFocus()
		Return
	ELSE
		dw_1.SetColumn("mosd_observ")
		dw_1.SetFocus()
	END IF
		
	dw_3.Reset()
	NoExistePallet(Long(istr_busq.Argum[2]))
ELSE
	dw_1.SetColumn("paen_numero")
	dw_1.SetFocus()
END IF

dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 6")

RETURN
end subroutine

event open;x				=	0
y				=	0
This.Height	=	2592
im_menu		=	m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_2.Object.plde_codigo[1] = gi_CodPlanta

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve(1)
dw_2.Object.clie_codigo[1] = gi_CodExport

dw_2.GetChild("sere_codigo", idwc_servicio)
idwc_servicio.SetTransObject(sqlca)
idwc_servicio.Retrieve()

dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)

istr_mant.dw2						=	dw_3

istr_mant.argumento[1]	=	String(gi_CodPlanta)
istr_mant.argumento[2]	=	String(gi_CodExport)
dw_2.Object.mose_fecmov[1] = Date(Today())
dw_2.Object.mose_hormov[1] = Time(Now())
dw_2.Object.mose_digita[1] = gstr_us.Nombre

im_menu		=	m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								

end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;String	ls_Calificacion, ls_CalCalidad, ls_CalCondicion, ls_CalEmbalaje, &
			ls_Mensaje,ls_Colu[]
Integer	li_Cont
Long		ll_Secuen

IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
END IF

IF dw_1.RowCount()	>	0	THEN	

	   il_fila = dw_1.InsertRow(0)
		dw_1.Setfocus()
		dw_1.ScrollToRow(il_fila)
		dw_1.SetRow(il_fila)
		dw_1.SetColumn("paen_numero")

ELSE
	IF dw_1.RowCount() =	0	THEN il_fila = dw_1.InsertRow(0)	
	
	dw_1.Setfocus()
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRow(il_fila)
	dw_1.SetColumn("paen_numero")		

END IF

dw_1.Object.clie_codigo[il_fila] = integer(istr_mant.argumento[2])
dw_1.Object.plde_codigo[il_fila] = integer(istr_mant.argumento[1])



end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Long(istr_mant.argumento[3]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
									
	ELSE
		DO
						
			ll_fila_d	= dw_1.Retrieve( Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[3]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled		= True
				pb_ins_det.Enabled	= True


				IF ll_fila_d > 0 THEN
					istr_mant.argumento[4] = String(dw_2.Object.sere_codigo[1])
					istr_mant.argumento[5] = String(dw_2.Object.serd_codigo[1])
				   pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					
				ELSE
					pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

dw_2.Enabled	=	False
end event

on w_maed_movtoserviadienca_planta.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
end on

on w_maed_movtoserviadienca_planta.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
end on

event ue_seleccion;call super::ue_seleccion;Long		ll_null

SetNull(ll_null)

istr_busq.argum[1]	=	String(dw_2.Object.plde_codigo[1])
istr_busq.argum[2]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[3]	= ''

OpenWithParm(w_busc_movtoservienca, istr_busq)

istr_busq	=	Message.PowerObjectParm

IF istr_busq.argum[3] <> "" THEN
	istr_mant.argumento[3]  = istr_busq.argum[3]
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_nuevo;call super::ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
		
//			IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
			IF dw_1.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", gi_CodExport)
dw_2.SetItem(1, "plde_codigo", gi_CodPlanta)

dw_2.GetChild("sere_codigo", idwc_servicio)
idwc_servicio.SetTransObject(sqlca)
idwc_servicio.Retrieve(Integer(gi_CodPlanta))

istr_mant.argumento[1]		=	String(gi_CodPlanta)
istr_mant.argumento[2]		=	String(gi_CodExport)
dw_2.Object.mose_fecmov[1] = Date(Today())
dw_2.Object.mose_hormov[1] = Time(Now())
dw_2.Object.mose_digita[1] = gstr_us.Nombre



end event

event ue_antesguardar;Long	ll_numero, ll_fila, ll_cont
Integer	li_Servicio, li_detalle

li_servicio = dw_2.Object.sere_codigo[1]
li_detalle  = dw_2.Object.serd_codigo[1]

istr_mant.argumento[1] = String(dw_2.Object.plde_codigo[1])
istr_mant.argumento[2] = String(dw_2.Object.clie_codigo[1])

IF isnull(dw_2.Object.mose_numero[1]) OR dw_2.Object.mose_numero[1] = 0 THEN
	ll_numero = existefolio(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]))
	dw_2.Object.mose_numero[1] = ll_numero
	
	IF ll_numero > 0 THEN
		FOR ll_fila = 1 TO dw_1.RowCount()
			dw_1.Object.mose_numero[ll_fila] = ll_numero
			dw_1.Object.sere_codigo[ll_fila] = li_servicio
			dw_1.Object.serd_codigo[ll_fila] = li_detalle
			istr_mant.argumento[3] = String(ll_numero)
		NEXT
		
		FOR ll_fila = 1 TO dw_3.RowCount()
			dw_3.Object.mose_numero[ll_fila] = ll_numero
			dw_3.Object.sere_codigo[ll_fila] = li_servicio
			dw_3.Object.serd_codigo[ll_fila] = li_detalle
			istr_mant.argumento[3] = String(dw_2.Object.mose_numero[1])
		NEXT	
		
	END IF
ELSE	
	ll_numero = dw_2.OBject.mose_numero[1]
	FOR ll_fila = 1 TO dw_1.RowCount()
		dw_1.Object.mose_numero[ll_fila] = ll_numero
		dw_1.Object.sere_codigo[ll_fila] = li_servicio
		dw_1.Object.serd_codigo[ll_fila] = li_detalle
		istr_mant.argumento[3] = String(ll_numero)
	NEXT
	
	FOR ll_fila = 1 TO dw_3.RowCount()
		dw_3.Object.mose_numero[ll_fila] = ll_numero
		dw_3.Object.sere_codigo[ll_fila] = li_servicio
		dw_3.Object.serd_codigo[ll_fila] = li_detalle
	NEXT	
END IF

FOR ll_cont = 1 TO dw_1.RowCount()
	IF dw_1.Object.paen_numero[ll_cont] = 0 OR isnull(dw_1.Object.paen_numero[ll_cont]) THEN
		MessageBox("Error","Falta Ingreso de Pallet en Detalle", Information!, Ok!)
		Message.DoubleParm = -1 
		Return
	END IF	
NEXT	





end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtoserviadienca_planta
integer x = 32
integer y = 500
integer width = 3561
integer height = 644
integer taborder = 100
boolean titlebar = false
string title = "Secuencias de Liquidación"
string dataobject = "dw_mues_movtoseviadideta_planta"
boolean hscrollbar = false
boolean livescroll = false
long il_selected_row = 5
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_Nula
Integer	li_cliente,pallet,li_status,li_tipo

SetNull(ls_Nula)

ls_columna = dwo.name

CHOOSE CASE ls_columna
			
	CASE "paen_numero"
		
		IF Duplicado(Long(data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			RETURN 1
		END IF
		
		IF NoExistePallet(Long(data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			RETURN 1
		END IF
		
		dw_3.Reset()

END CHOOSE
end event

event dw_1::buttonclicked;call super::buttonclicked;Integer	li_Secuen, li_Zona, li_copiapo,li_ovalle,li_vicuna, &
			li_vallenar,li_aconcagua,li_surmetro,li_surrancagua

Date		ld_FDesde, ld_FHasta

dw_3.Reset()

CHOOSE CASE dwo.Name
	CASE "genera"
			recupera_caracteristicas(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_1.Object.paen_numero[row])
END CHOOSE
end event

event dw_1::clicked;call super::clicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "buscapallet"
		BuscaPallet()
		
END CHOOSE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtoserviadienca_planta
integer x = 741
integer y = 16
integer width = 2560
integer height = 456
integer taborder = 10
string dataobject = "dw_mant_movtoserviadienca"
end type

event dw_2::itemchanged;call super::itemchanged;Integer	li_Planta
Long		ll_null
String	ls_columna
Date		ld_nula

SetNull(ld_nula)
SetNull(ll_null)
ls_columna = dwo.Name
 
CHOOSE CASE ls_columna
	CASE "clie_codigo"
			
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		END IF			
		istr_mant.argumento[2]=data
		
	CASE "mose_numero"
			
		IF existeregistro(Integer(istr_mant.argumento[2]),integer(istr_mant.argumento[1]),LOng(Data)) THEN
			istr_mant.argumento[3] = Data
			w_maed_movtoserviadienca_planta.TriggerEvent("ue_recuperadatos")
		ELSE	
			MessageBox("Atención", "Número no Existe. Ingrese otro.")
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF			

	CASE "plde_codigo"
		IF existe_planta(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, Integer(ll_null))
			RETURN 1
		END IF	
				
		istr_mant.argumento[1] = data
		
	CASE "sere_codigo"
		IF noexisteservicios(ls_columna, Integer(data)) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF	
		
		istr_mant.argumento[4] = data
		
		dw_2.GetChild("serd_codigo", idwc_detalle)
		idwc_detalle.SetTransObject(sqlca)
		idwc_detalle.Retrieve(integer(data))
		
	CASE "serd_codigo"	
		IF noexisteservicios(ls_columna, Integer(data)) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF	
		istr_mant.argumento[5] = data
		
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//
end event

event dw_2::itemerror;call super::itemerror;RETURN 1
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtoserviadienca_planta
integer x = 4539
integer y = 336
integer taborder = 40
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtoserviadienca_planta
integer x = 4539
integer y = 736
integer taborder = 50
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtoserviadienca_planta
integer x = 4539
integer y = 936
integer taborder = 60
boolean enabled = true
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtoserviadienca_planta
integer x = 4539
integer y = 536
integer taborder = 70
boolean enabled = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtoserviadienca_planta
integer x = 4539
integer y = 1136
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtoserviadienca_planta
integer x = 4539
integer taborder = 20
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtoserviadienca_planta
integer x = 4539
integer y = 1816
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtoserviadienca_planta
integer x = 4539
integer y = 136
integer taborder = 30
end type

type dw_3 from datawindow within w_maed_movtoserviadienca_planta
integer y = 1160
integer width = 4009
integer height = 860
integer taborder = 80
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_mues_movtoservicioscaracteristicas"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF Row > 0 THEN
	il_fila1 = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila1)
	This.SelectRow(il_fila1,True)
END IF

RETURN 0
end event

type dw_4 from datawindow within w_maed_movtoserviadienca_planta
boolean visible = false
integer y = 32
integer width = 686
integer height = 400
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_palletmovtoservicioscaracteristicas"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

