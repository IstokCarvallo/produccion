$PBExportHeader$w_maed_spro_lotefrutagranel.srw
forward
global type w_maed_spro_lotefrutagranel from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_spro_lotefrutagranel
end type
type tp_1 from userobject within tab_1
end type
type dw_madurez from datawindow within tp_1
end type
type pb_inserta_mc from picturebutton within tp_1
end type
type pb_elimina_mc from picturebutton within tp_1
end type
type tp_1 from userobject within tab_1
dw_madurez dw_madurez
pb_inserta_mc pb_inserta_mc
pb_elimina_mc pb_elimina_mc
end type
type tp_2 from userobject within tab_1
end type
type dw_colorfondo from datawindow within tp_2
end type

type tp_2 from userobject within tab_1
dw_colorfondo dw_colorfondo
end type
type tp_3 from userobject within tab_1
end type
type dw_cubrim from datawindow within tp_3
end type
type tp_3 from userobject within tab_1
dw_cubrim dw_cubrim
end type
type tp_4 from userobject within tab_1
end type
type dw_calibre from datawindow within tp_4
end type
type tp_4 from userobject within tab_1
dw_calibre dw_calibre
end type
type tp_5 from userobject within tab_1
end type
type dw_danos from datawindow within tp_5
end type
type tp_5 from userobject within tab_1
dw_danos dw_danos
end type
type tp_5a from userobject within tab_1
end type
type pb_elimina_doc from picturebutton within tp_5a
end type
type pb_inserta_doc from picturebutton within tp_5a
end type
type dw_otrosdanos from datawindow within tp_5a
end type
type tp_5a from userobject within tab_1
pb_elimina_doc pb_elimina_doc
pb_inserta_doc pb_inserta_doc
dw_otrosdanos dw_otrosdanos
end type
type tp_6 from userobject within tab_1
end type
type dw_lotedeta from datawindow within tp_6
end type
type tp_6 from userobject within tab_1
dw_lotedeta dw_lotedeta
end type
type tab_1 from tab within w_maed_spro_lotefrutagranel
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
tp_4 tp_4
tp_5 tp_5
tp_5a tp_5a
tp_6 tp_6
end type
end forward

global type w_maed_spro_lotefrutagranel from w_mant_encab_deta_csd
integer width = 3589
integer height = 2052
string title = "CONTROL DE CALIDAD RECEPCION"
string menuname = ""
event ue_imprimir ( )
tab_1 tab_1
end type
global w_maed_spro_lotefrutagranel w_maed_spro_lotefrutagranel

type variables
uo_productores			iuo_productores
uo_condicioncc			iuo_CondiCC
uo_lotesfrutagranel	iuo_Lotes
uo_colordefondo		iuo_ColorFondo
uo_espevarigrucal		iuo_EspevariGrucal
uo_especiecatego     iuo_EspecieCatego
uo_DanosyDefectos		iuo_Danos
uo_variedades			iuo_variedad
uo_ParamMadurez	   iuo_parammadurez

Boolean							ib_modifica
DataWindowChild   			dw_especies, idwc_planta, idwc_especie, idwc_variedad, &
									idwc_tipofrio, idwc_periodofrio, idwc_condicion, &
									idwc_categoria, idwc_color_fondo, idwc_danos, &
									idwc_color_fondo1
									
DataWindow						dw_3, dw_4, dw_5, dw_6, dw_7, dw_8, dw_9
str_variedad					istr_variedad
str_categoria					istr_categoria

String							is_columna

DataStore			ids_Base
end variables

forward prototypes
public function boolean duplicadocalibre (string campo, integer tipo)
public function boolean duplicadocolor (string campo, integer tipo)
public function boolean duplicadootrosdanos (string as_columna, string as_valor)
public subroutine buscaproductor ()
public function boolean ascendenciadano (integer ai_nivelc, integer ai_tipodd, integer ai_codigo, decimal ad_porcen, integer ai_muestra)
public function boolean existeespecie (string as_valor)
public subroutine habilitacolumnas (boolean habilita)
public subroutine habilitadetalle (boolean habilita)
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine existedano ()
public subroutine generadetalle ()
end prototypes

event ue_imprimir();str_busqueda	lstr_busq

lstr_busq.Argum[1]	=	istr_mant.Argumento[1]
lstr_busq.Argum[2]	=	istr_mant.Argumento[2]
lstr_busq.Argum[3]	=	istr_mant.Argumento[3]
lstr_busq.Argum[4]	=	istr_mant.Argumento[4]

OpenWithParm(w_info_opcion_ctrol_cali_recepcion, lstr_busq)
end event

public function boolean duplicadocalibre (string campo, integer tipo);Long		ll_fila
String	disca_grupca

disca_grupca	=	String(dw_6.Object.disca_grupca[il_fila])

CHOOSE CASE tipo

	CASE 1
		disca_grupca	=	Campo

END CHOOSE

ll_fila = dw_6.Find("disca_grupca = " + disca_grupca , 1, dw_6.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, OK!)
   RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean duplicadocolor (string campo, integer tipo);Long		ll_fila
String	cofo_codigo

cofo_codigo	=	String(dw_4.Object.cofo_codigo[il_fila])

CHOOSE CASE tipo

	CASE 1
		cofo_codigo	=	Campo

END CHOOSE

ll_fila = dw_4.Find("cofo_codigo = " + cofo_codigo + "", 1, dw_4.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, OK!)
   RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean duplicadootrosdanos (string as_columna, string as_valor);Long		ll_fila
Integer	li_NivelC, li_Tipodd, li_Codigo
String	disca_grupca

li_NivelC	=	dw_9.Object.dade_nivcal[il_fila]
li_Tipodd	=	dw_9.Object.dade_tipodd[il_fila]
li_Codigo	=	dw_9.Object.dade_codigo[il_Fila]

CHOOSE CASE as_columna

	CASE "dade_nivcal"
		li_NivelC	=	Integer(as_valor)
		
	CASE "dade_tipodd"
		li_Tipodd	=	Integer(as_valor)
		
	CASE "dade_codigo"
		li_Codigo	=	Integer(as_valor)

END CHOOSE

IF Isnull(li_NivelC) OR Isnull(li_Tipodd) OR Isnull(li_Codigo) THEN RETURN False

ll_fila =	dw_9.Find("dade_nivcal = " + String(li_NivelC) + " and " + &
							 "dade_tipodd = " + String(li_Tipodd) + " and " + &
							 "dade_codigo = " + String(li_Codigo), 1, dw_9.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, OK!)
   RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine buscaproductor ();OpenWithParm(w_busc_productores, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[1] = "" THEN
	dw_2.SetColumn("prod_codigo")
	dw_2.SetFocus()
ELSE
	dw_2.Object.prod_codigo[1]	=	Long(istr_busq.argum[1])
	dw_2.Object.prod_nombre[1]	=	istr_busq.argum[2]
END IF

RETURN
end subroutine

public function boolean ascendenciadano (integer ai_nivelc, integer ai_tipodd, integer ai_codigo, decimal ad_porcen, integer ai_muestra);Long		ll_Fila
Integer	li_Nivel
Dec{2}	ld_OtroPorc

ll_Fila	=	dw_7.Find("dade_tipodd = "+String(ai_Tipodd)+" and "+&
							 "dade_codigo = "+String(ai_codigo),1,dw_7.RowCount())
							 
IF ll_Fila = 0 THEN
	MessageBox("Atención","Daño no está evaluado para el Nivel de Calidad Principal")
	RETURN False
ELSE
	IF ai_muestra = 1 THEN 
		ld_OtroPorc	=	dw_7.Object.dade_podade[ll_Fila]
	ELSE
		ld_OtroPorc	=	dw_7.Object.dade_poddpr[ll_Fila]
	END IF
	
	IF ld_OtroPorc < ad_Porcen THEN
		MessageBox("Atención","Porcentaje de Dano en Nivel Principal es menor al registrado.~r~n"+&
					  "Supone una mayor tolerancia al daño en los niveles inferiores.")
		RETURN False
	END IF
END IF

FOR li_Nivel = ai_NivelC - 1 TO 2 Step -1
	ll_Fila	=	dw_9.Find("dade_nivcal = " + String(li_Nivel) + " and " + &
								 "dade_tipodd = " + String(ai_Tipodd) + " and " + &
								 "dade_codigo = " + String(ai_codigo), 1, dw_9.RowCount())
	IF ll_Fila = 0 THEN
		MessageBox("Atención","Daño no está evaluado para el Nivel de Calidad Principal")
		RETURN False
	ELSE
		IF ai_muestra = 1 THEN 
			ld_OtroPorc	=	dw_9.Object.dade_podade[ll_Fila]
		ELSE
			ld_OtroPorc	=	dw_9.Object.dade_poddpr[ll_Fila]
		END IF
		
		IF ld_OtroPorc < ad_Porcen THEN
			MessageBox("Atención","Porcentaje de Dano en Nivel Superior es menor al registrado.~r~n"+&
						  "Supone una mayor tolerancia al daño en los niveles inferiores.")
			RETURN False
		END IF
	END IF
NEXT

RETURN TRUE
end function

public function boolean existeespecie (string as_valor);Integer	espe_codigo
String	nombre

espe_codigo	= Integer(as_valor)
		

IF IsNull(espe_codigo) = False THEN
	istr_mant.argumento[2]	= String(espe_codigo)
	
	 SELECT espe_nombre
    INTO :nombre 
    FROM  dba.especies 
   WHERE  espe_codigo = :espe_codigo;

	IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Especies" )
	ELSEIF sqlca.SQLCode = 0 THEN
		RETURN TRUE
	ELSE
			dw_especies.setitem(1, "espe_codigo", 1)
	END IF
END IF
RETURN FALSE

end function

public subroutine habilitacolumnas (boolean habilita);IF Habilita THEN
	dw_2.object.lote_espcod.Protect  			=  1
	dw_2.Object.lote_espcod.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.vari_codigo.Protect				=	0
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.frio_tipofr.Protect				=	0
	dw_2.Object.frio_tipofr.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.pefr_codigo.Protect				=	0
	dw_2.Object.pefr_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cocc_codigo.Protect				=	0
	dw_2.Object.cocc_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cate_codigo.Protect				=	0
	dw_2.Object.cate_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_nrofol.Protect				=	0
	dw_2.Object.fgcc_nrofol.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_feccon.Protect				=	0
	dw_2.Object.fgcc_feccon.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_horcon.Protect				=	0
	dw_2.Object.fgcc_horcon.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_llenop.Protect				=	0
	dw_2.Object.fgcc_llenop.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_encarp.Protect				=	0
	dw_2.Object.fgcc_encarp.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_empper.Protect				=	0
	dw_2.Object.fgcc_empper.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.fgcc_observ.Protect				=	0
	dw_2.Object.fgcc_observ.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cmen_califi.Protect				=	0
	dw_2.Object.cmen_califi.BackGround.Color	=	RGB(255,255,255)
ELSE
	dw_2.object.lote_espcod.Protect  			=  0
	dw_2.Object.lote_espcod.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.vari_codigo.Protect				=	1
	dw_2.Object.vari_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.frio_tipofr.Protect				=	1
	dw_2.Object.frio_tipofr.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.pefr_codigo.Protect				=	1
	dw_2.Object.pefr_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.cocc_codigo.Protect				=	1
	dw_2.Object.cocc_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.cate_codigo.Protect				=	1
	dw_2.Object.cate_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_nrofol.Protect				=	1
	dw_2.Object.fgcc_nrofol.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_feccon.Protect				=	1
	dw_2.Object.fgcc_feccon.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_horcon.Protect				=	1
	dw_2.Object.fgcc_horcon.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_llenop.Protect				=	1
	dw_2.Object.fgcc_llenop.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_encarp.Protect				=	1
	dw_2.Object.fgcc_encarp.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_empper.Protect				=	1
	dw_2.Object.fgcc_empper.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.fgcc_observ.Protect				=	1
	dw_2.Object.fgcc_observ.BackGround.Color	=	RGB(192,192,192)
	dw_2.Object.cmen_califi.Protect				=	1
	dw_2.Object.cmen_califi.BackGround.Color	=	RGB(192,192,192)
END IF
end subroutine

public subroutine habilitadetalle (boolean habilita);tab_1.tp_1.Enabled	=	Habilita
tab_1.tp_2.Enabled	=	Habilita
tab_1.tp_3.Enabled	=	Habilita
tab_1.tp_4.Enabled	=	Habilita
tab_1.tp_5.Enabled	=	Habilita
tab_1.tp_5a.Enabled	=	Habilita
tab_1.tp_6.Enabled	=	Habilita
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
//	dw_2.SetTabOrder("espe_codigo",10)
//	dw_2.Modify("espe_codigo.BackGround.Color = " + String(rgb(255,255,255)))
//	dw_2.SetTabOrder("grva_codigo",20)
//	dw_2.Modify("grva_codigo.BackGround.Color = " + String(rgb(255,255,255)))
//	dw_2.SetTabOrder("grva_nombre",30)
//	dw_2.Modify("grva_nombre.BackGround.Color = " + String(rgb(255,255,255)))
ELSE
//	dw_2.SetTabOrder("espe_codigo",0)
//	dw_2.Modify("espe_codigo.BackGround.Color = " + String(rgb(192,192,192)))
//	dw_2.SetTabOrder("grva_codigo",0)
//	dw_2.Modify("grva_codigo.BackGround.Color = " + String(rgb(192,192,192)))
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_3.Update(True, False) = 1 THEN
		IF dw_4.Update(True, False) = 1 THEN
			IF dw_5.Update(True, False) = 1 THEN
				IF dw_6.Update(True, False) = 1 THEN
					IF dw_7.Update(True, False) = 1 THEN
						IF dw_9.Update(True, False) = 1 THEN
								Commit;
					
								IF sqlca.SQLCode <> 0 THEN
									F_ErrorBaseDatos(sqlca, This.Title)
									Rollback;
								ELSE
									lb_Retorno	=	True
					
									dw_3.ResetUpdate()
									dw_4.ResetUpdate()
									dw_5.ResetUpdate()
									dw_6.ResetUpdate()
									dw_7.ResetUpdate()
									dw_9.ResetUpdate()
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
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				IF dw_5.Update(True, False) = 1 THEN
					IF dw_6.Update(True, False) = 1 THEN
						IF dw_7.Update(True, False) = 1 THEN
							IF dw_9.Update(True, False) = 1 THEN
								
								Commit;
						
								IF sqlca.SQLCode <> 0 THEN
									F_ErrorBaseDatos(sqlca, This.Title)
									Rollback;
								ELSE
									lb_Retorno	=	True
						
									dw_2.ResetUpdate()
									dw_3.ResetUpdate()
									dw_4.ResetUpdate()
									dw_5.ResetUpdate()
									dw_6.ResetUpdate()
									dw_7.ResetUpdate()
									dw_9.ResetUpdate()
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

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine existedano ();Integer li_pos, li_codigo, li_fila, li_null, li_grupo, li_subgrupo
String  ls_nom, ls_codigo
Decimal ld_por

SEtNull(li_null)


FOR li_pos = 1 TO dw_4.rowcount()
	li_codigo = dw_4.object.cofo_codigo[li_pos]
	IF iuo_variedad.existe(Integer(istr_Mant.Argumento[2]),Integer(istr_Mant.Argumento[4]),&
	                       TRUE,SQLCA) THEN
		li_grupo 	= 	iuo_variedad.grupo
		li_subgrupo	=	iuo_variedad.subgrupo
	ELSE
		SetNull(li_grupo)
		SetNull(li_subgrupo)
	END IF 
	
	IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), li_grupo, li_subgrupo, &
									Integer(istr_Mant.Argumento[4]), &
									li_codigo, False, Sqlca) THEN
		IF iuo_ColorFondo.Dano = 1 AND dw_4.Object.cfre_porcol[li_pos] > 0 THEN
			li_fila = dw_7.insertrow(0)
			dw_7.setitem(li_fila,"dade_tipodd",3)
			dw_7.setitem(li_fila,"dade_codigo",0)
			dw_7.setitem(li_fila,"dade_nombre",iuo_ColorFondo.Nombre)
			dw_7.SetItem(li_fila,"dano_altern",String(li_Codigo))
			dw_7.setitem(li_fila,"dade_podade",dw_4.Object.cfre_porcol[li_pos])
		END IF
	ELSE
		IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), li_grupo, li_subgrupo, &
									-1, li_codigo, False, Sqlca) THEN
			IF iuo_ColorFondo.Dano = 1 AND dw_4.Object.cfre_porcol[li_pos] > 0 THEN
				li_fila = dw_7.insertrow(0)
				dw_7.setitem(li_fila,"dade_tipodd",3)
				dw_7.setitem(li_fila,"dade_codigo",0)
				dw_7.setitem(li_fila,"dade_nombre",iuo_ColorFondo.Nombre)
				dw_7.SetItem(li_fila,"dano_altern",String(li_Codigo))
				dw_7.setitem(li_fila,"dade_podade",dw_4.Object.cfre_porcol[li_pos])
			END IF
		ELSE
			IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), li_grupo, -1, &
											-1, li_codigo, False, Sqlca) THEN
				IF iuo_ColorFondo.Dano = 1 AND dw_4.Object.cfre_porcol[li_pos] > 0 THEN
					li_fila = dw_7.insertrow(0)
					dw_7.setitem(li_fila,"dade_tipodd",3)
					dw_7.setitem(li_fila,"dade_codigo",0)
					dw_7.setitem(li_fila,"dade_nombre",iuo_ColorFondo.Nombre)
					dw_7.SetItem(li_fila,"dano_altern",String(li_Codigo))
					dw_7.setitem(li_fila,"dade_podade",dw_4.Object.cfre_porcol[li_pos])
				END IF
			ELSE
				IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), -1, -1, &
												-1, li_codigo, False, Sqlca) THEN
					IF iuo_ColorFondo.Dano = 1 AND dw_4.Object.cfre_porcol[li_pos] > 0 THEN
						li_fila = dw_7.insertrow(0)
						dw_7.setitem(li_fila,"dade_tipodd",3)
						dw_7.setitem(li_fila,"dade_codigo",0)
						dw_7.setitem(li_fila,"dade_nombre",iuo_ColorFondo.Nombre)
						dw_7.SetItem(li_fila,"dano_altern",String(li_Codigo))
						dw_7.setitem(li_fila,"dade_podade",dw_4.Object.cfre_porcol[li_pos])
					END IF
				END IF	
			END IF	
		END IF
	END IF 
NEXT

FOR li_pos = 1 TO dw_5.rowcount()
	 li_codigo = dw_5.object.cate_codigo[li_pos]

	 IF iuo_EspecieCatego.Existe(Integer(istr_Mant.Argumento[2]), &
									Integer(istr_Mant.Argumento[4]), &
									li_codigo, False, Sqlca) THEN
		IF iuo_EspecieCatego.Dano = 1 AND dw_5.Object.ccca_porcen[li_pos] > 0 THEN
			li_fila = dw_7.insertrow(0)
			dw_7.setitem(li_fila,"dade_tipodd",4)
			dw_7.setitem(li_fila,"dade_codigo",0)
			dw_7.setitem(li_fila,"dade_nombre",dw_5.object.categoria[li_pos])
			dw_7.SetItem(li_fila,"dano_altern",String(li_Codigo))
			dw_7.setitem(li_fila,"dade_podade",dw_5.Object.ccca_porcen[li_pos])
		END IF
		
	ELSE
		
		IF iuo_EspecieCatego.Existe(Integer(istr_Mant.Argumento[2]), &
									li_null,li_codigo, False, Sqlca) THEN
			IF iuo_EspecieCatego.Dano = 1 AND dw_5.Object.ccca_porcen[li_pos] > 0 THEN
				li_fila = dw_7.insertrow(0)
				dw_7.setitem(li_fila,"dade_tipodd",4)
				dw_7.setitem(li_fila,"dade_codigo",0)
				dw_7.setitem(li_fila,"dade_nombre",dw_5.object.categoria[li_pos])
				dw_7.SetItem(li_fila,"dano_altern",String(li_Codigo))
				dw_7.setitem(li_fila,"dade_podade",dw_5.Object.ccca_porcen[li_pos])
			END IF
		
		END IF
		
	END IF 
	 
NEXT


FOR li_pos = 1 TO dw_6.rowcount()
	 
	 ls_codigo = dw_6.object.disca_grupca[li_pos]
	 
	 IF iuo_EspevariGrucal.Existe(Integer(istr_Mant.Argumento[2]), &
									Integer(istr_Mant.Argumento[4]), &
									ls_codigo, False, Sqlca) THEN
		IF iuo_EspevariGrucal.Dano = 1 AND dw_6.Object.disca_porren[li_pos] > 0 THEN
			li_fila = dw_7.insertrow(0)
			dw_7.setitem(li_fila,"dade_tipodd",5)
			dw_7.setitem(li_fila,"dade_codigo",0)
			dw_7.setitem(li_fila,"dade_nombre",dw_6.object.glosa[li_pos])
			dw_7.SetItem(li_fila,"dano_altern",ls_Codigo)
			dw_7.setitem(li_fila,"dade_podade",dw_6.Object.disca_porren[li_pos])
		END IF
	ELSE 
		IF iuo_EspevariGrucal.Existe(Integer(istr_Mant.Argumento[2]), &
									li_null, ls_codigo, False, Sqlca) THEN
			IF iuo_EspevariGrucal.Dano = 1 AND dw_6.Object.disca_porren[li_pos] > 0 THEN
				li_fila = dw_7.insertrow(0)
				dw_7.setitem(li_fila,"dade_tipodd",5)
				dw_7.setitem(li_fila,"dade_codigo",0)
				dw_7.setitem(li_fila,"dade_nombre",dw_6.object.glosa[li_pos])
				dw_7.SetItem(li_fila,"dano_altern",ls_Codigo)
				dw_7.setitem(li_fila,"dade_podade",dw_6.Object.disca_porren[li_pos])
			END IF
		
		END IF 	 
		
	END IF
	
NEXT
end subroutine

public subroutine generadetalle ();Long		ll_Fila, ll_Filas, ll_FilaNueva
Integer	li_Null, li_Contador, li_grupo, li_subgrupo, li_colorfondo
String   ls_Mensaje
Boolean  lb_existe = False

SetNull(li_Null)
ll_FilaNueva = 0
//	Llena Color de Fondo

IF dw_4.RowCount() >= 0 THEN
	
	IF iuo_variedad.existe(Integer(istr_Mant.Argumento[2]),Integer(istr_Mant.Argumento[4]),&
	                       TRUE,SQLCA) THEN
								  
		li_grupo 	= 	iuo_variedad.grupo
		li_subgrupo	=	iuo_variedad.subgrupo
	ELSE
		SetNull(li_grupo)
		SetNull(li_subgrupo)
	END IF	
	
	IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),li_grupo,li_subgrupo,&
											Integer(istr_Mant.Argumento[4]), &
											FALSE, Sqlca) THEN
		IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),li_grupo,li_subgrupo,&
											-1, &
											FALSE, Sqlca) THEN
			IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),li_grupo,-1,&
											-1, &
											FALSE, Sqlca) THEN
				IF NOT iuo_ParamMadurez.Existe(Integer(istr_Mant.Argumento[2]),-1,-1,&
											-1, &
											True, Sqlca) THEN
			   	lb_existe = FALSE
				ELSE
					lb_existe = TRUE
				END IF
			ELSE
				lb_existe = TRUE	
			END IF
		ELSE
			lb_existe = TRUE	
		END IF
	ELSE
 		lb_existe = TRUE	
	END IF	
	
	IF lb_existe THEN	
	
		IF iuo_ParamMadurez.Color = 0 THEN
			li_colorfondo = 0
		ElSE
	   	li_colorfondo = 1
	
		END IF	
	
		IF li_colorfondo = 1 THEN	
			ids_Base.DataObject	=	"dw_sele_spro_colordefondo"
			ids_Base.SetTransObject(sqlca)
				
			ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]), li_grupo, li_subgrupo, &
													Integer(istr_Mant.Argumento[4]))
			
			IF ll_Filas = 0 THEN
			
				ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]), li_grupo, li_subgrupo, &
														-1)
			END IF											
				
			IF ll_Filas = 0 THEN
				
				ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]), li_grupo, -1, &
														-1)
			END IF												
				
			IF ll_Filas = 0 THEN
				
				ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]), -1, -1, &
														-1)
			END IF
			
			IF ll_Filas = 0 THEN
				li_Contador ++
				ls_Mensaje      =ls_mensaje + ("~rColor de Fondo ")
			ELSE
				FOR ll_Filas = 1 TO ids_Base.RowCount()
					ll_FilaNueva = 0 
					ll_FilaNueva = dw_4.Find("cofo_codigo = " + String(ids_Base.Object.cofo_codigo[ll_Filas]), &
													  1, dw_4.RowCount()) 
													  
					IF ll_FilaNueva <=0 THEN								  
						ll_Fila	=	dw_4.InsertRow(0)
					
						dw_4.Object.cofo_codigo[ll_Fila]	=	ids_Base.Object.cofo_codigo[ll_Filas]
					END IF	
				NEXT
			END IF
		END IF
	END IF 	
END IF

//	Llena Color de Cubrimiento
ll_FilaNueva = 0
IF dw_5.RowCount() >= 0 THEN
	ids_Base.DataObject	=	"dw_mues_spro_especiecatego"
	ids_Base.SetTransObject(sqlca)
	
	ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]),&
											Integer(istr_Mant.Argumento[4]))
	
	IF ll_Filas = 0 THEN
		ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]),&
												li_Null)
	END IF

	IF ll_Filas = 0 THEN
		li_Contador ++
		ls_Mensaje      =ls_mensaje + ("~rColores de Cubrimiento ")
	ELSE
		FOR ll_Filas = 1 TO ids_Base.RowCount()
			ll_FilaNueva = 0 
			ll_FilaNueva = dw_5.Find("cate_codigo = " + String(ids_Base.Object.cate_codigo[ll_Filas]), &
			                          1, dw_5.RowCount())
			
			IF ll_FilaNueva <= 0 THEN
				ll_Fila	=	dw_5.InsertRow(0)
			
				dw_5.Object.cate_nombre[ll_Fila]	=	ids_Base.Object.cate_nombre[ll_Filas]
				dw_5.Object.esca_despor[ll_Fila]	=	ids_Base.Object.esca_despor[ll_Filas]
				dw_5.Object.cate_codigo[ll_Fila]	=	ids_Base.Object.cate_codigo[ll_Filas]
			END IF	
		NEXT
	END IF
END IF

//	Llena Distribución de Calibre
ll_FilaNueva = 0	
IF dw_6.RowCount() >= 0 THEN
	ids_Base.DataObject	=	"dw_mues_spro_espevarigrucal"
	ids_Base.SetTransObject(sqlca)
	
	ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]),&
											Integer(istr_Mant.Argumento[4]))
	
	IF ll_Filas = 0 THEN
		ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]),&
												li_Null)
	END IF

	IF ll_Filas = 0 THEN
		li_Contador ++
		ls_Mensaje      =ls_mensaje + ("~rDistribución de Calibre ")
	ELSE
		FOR ll_Filas = 1 TO ids_Base.RowCount()
			ll_FilaNueva = 0 
			ll_FilaNueva = dw_6.Find("disca_grupca = '" + ids_Base.Object.evdc_grucal[ll_Filas] + "'", &
			                          1, dw_6.RowCount())
			IF ll_FilaNueva <= 0 THEN								  
				ll_Fila	=	dw_6.InsertRow(0)
			
				dw_6.Object.disca_grupca[ll_Fila]	=	ids_Base.Object.evdc_grucal[ll_Filas]
				dw_6.Object.evdc_despor[ll_Fila]		=	ids_Base.Object.evdc_despor[ll_Filas]
			END IF	
		NEXT
	END IF
END IF

//	Llena Daños y Defectos
IF dw_7.RowCount() >= 0 THEN
	ids_Base.DataObject	=	"dw_mues_danosydefectos"
	ids_Base.SetTransObject(sqlca)
		
	ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]),&
											Integer(istr_Mant.Argumento[4]))

	IF ll_Filas = 0 THEN
		ll_Filas	=	ids_Base.Retrieve(Integer(istr_Mant.Argumento[2]),&
											li_Null)
	END IF
	
	IF ll_Filas = 0 THEN
      li_Contador ++ 
		ls_Mensaje      =ls_mensaje + ("~rDaños y Defectos")		

	ELSE
		FOR ll_Filas = 1 TO ids_Base.RowCount()
			ll_FilaNueva = 0 
			ll_FilaNueva = dw_7.Find("dade_tipodd = " + String(ids_Base.Object.dade_tipodd[ll_Filas]) + " AND " + &
			                         "dade_codigo = " + String(ids_Base.Object.dade_codigo[ll_Filas]), &
											 1 , dw_7.RowCount()) 
			
			IF ll_FilaNueva<=0 THEN
				ll_Fila	=	dw_7.InsertRow(0)
				
				dw_7.Object.dade_tipodd[ll_Fila]	=	ids_Base.Object.dade_tipodd[ll_Filas]
				dw_7.Object.dade_codigo[ll_Fila]	=	ids_Base.Object.dade_codigo[ll_Filas]
				dw_7.Object.dade_nombre[ll_Fila]	=	ids_Base.Object.dade_nombre[ll_Filas]
			END IF	
		NEXT
	END IF
END IF

IF li_Contador > 0 THEN
	MessageBox("Atención", "No se han creado "+ls_mensaje+" ~r" + &
					"para Especie indicada.~r~rIngrese o seleccione " + &
					"otra Variedad.")
END IF
end subroutine

event open;/* Argumentos

istr_mant.argumento[1] = Código Planta
istr_mant.argumento[2] = Código Especie
istr_mant.argumento[3] = Número Lote
istr_mant.argumento[4] = Código Variedad
*/

istr_Mant.Argumento[1]	=	String(gstr_paramplanta.codigoplanta)
istr_Mant.Argumento[2]	=	String(gstr_paramplanta.codigoespecie)
istr_Mant.Argumento[3]	=	''
istr_Mant.Argumento[4]	=	''

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

dw_2.GetChild("lote_pltcod", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

dw_2.GetChild("lote_espcod", idwc_especie)
idwc_especie.SetTransObject(SqlCa)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF

dw_2.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(SqlCa)
IF idwc_variedad.Retrieve(gstr_paramplanta.codigoespecie) = 0 THEN
	MessageBox("Atención","Falta Registrar Variedades")
	idwc_variedad.InsertRow(0)
END IF

dw_2.GetChild("frio_tipofr", idwc_tipofrio)
idwc_tipofrio.SetTransObject(SqlCa)
IF idwc_tipofrio.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tipo de Frío")
	idwc_tipofrio.InsertRow(0)
END IF

dw_2.GetChild("pefr_codigo", idwc_periodofrio)
idwc_periodofrio.SetTransObject(SqlCa)
IF idwc_periodofrio.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Periodos de Frío")
	idwc_periodofrio.InsertRow(0)
END IF

dw_2.GetChild("cocc_codigo", idwc_condicion)
idwc_condicion.SetTransObject(SqlCa)
IF idwc_condicion.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Condición")
	idwc_condicion.InsertRow(0)
END IF

dw_2.GetChild("cate_codigo", idwc_categoria)
idwc_categoria.SetTransObject(SqlCa)
IF idwc_categoria.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Condición")
	idwc_categoria.InsertRow(0)
END IF

dw_3	=	tab_1.tp_1.dw_madurez
dw_4	=	tab_1.tp_2.dw_colorfondo
dw_5	=	tab_1.tp_3.dw_cubrim
dw_6	=	tab_1.tp_4.dw_calibre
dw_7	=	tab_1.tp_5.dw_danos
dw_9	=	tab_1.tp_5a.dw_otrosdanos
dw_8  =  tab_1.tp_6.dw_lotedeta

dw_3.GetChild("cofo_codigo", idwc_color_fondo)
idwc_color_fondo.SetTransObject(SqlCa)
idwc_color_fondo.InsertRow(0)

dw_4.GetChild("cofo_codigo", idwc_color_fondo1)
idwc_color_fondo1.SetTransObject(SqlCa)
idwc_color_fondo1.InsertRow(0)

dw_7.GetChild("dade_codigo", idwc_danos)
idwc_danos.SetTransObject(SqlCa)
idwc_danos.InsertRow(0)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)
dw_8.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

pb_nuevo.PostEvent(Clicked!)

iuo_productores	=	Create uo_productores
iuo_CondiCC			=	Create uo_condicioncc
iuo_Lotes			=	Create uo_lotesfrutagranel
iuo_ColorFondo		=	Create uo_colordefondo
iuo_EspecieCatego	=	Create uo_especieCatego
iuo_EspevariGrucal=  Create uo_espevarigrucal
iuo_Danos			=	Create uo_DanosyDefectos
iuo_variedad		=  Create uo_variedades
iuo_ParamMadurez	=  Create uo_ParamMadurez
ids_Base				=	Create DataStore
end event

event ue_recuperadatos();Long	ll_fila_d, ll_fila_e, respuesta, ll_filas
Integer li_grupo, li_subgrupo, li_Null

SetNull(li_Null)

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()

	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), &
										 Integer(istr_mant.argumento[2]), &
										 Integer(istr_mant.argumento[3]))


	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		HabilitaColumnas(True)
		istr_mant.argumento[4] =  String(dw_2.object.vari_codigo[ll_fila_e])
		
		IF idwc_danos.Retrieve(Integer(istr_mant.argumento[2]),0) = 0 THEN
			MessageBox("Atención","Falta Registrar Daños y defectos para la Especie")
			idwc_danos.InsertRow(0)
		END IF

		DO
			IF Isnull(dw_2.Object.fgcc_feccon[1]) THEN
				dw_2.Object.fgcc_feccon[1]	=	Date(Today())
				dw_2.Object.fgcc_horcon[1]	=	Time(Now())
			END IF
         
			IF iuo_variedad.existe(Integer(istr_Mant.Argumento[2]),Integer(istr_Mant.Argumento[4]),&
	                       TRUE,SQLCA) THEN
				li_grupo 	= 	iuo_variedad.grupo
				li_subgrupo	=	iuo_variedad.subgrupo
			ELSE
				SetNull(li_grupo)
				SetNull(li_subgrupo)
			END IF
			
			IF idwc_color_fondo.Retrieve(Integer(istr_mant.argumento[2]),li_grupo,li_subgrupo, &
			                             Integer(istr_mant.argumento[4])) = 0 THEN
				IF idwc_color_fondo.Retrieve(Integer(istr_mant.argumento[2]),li_grupo,li_subgrupo, &
			                                -1) = 0 THEN	
					IF idwc_color_fondo.Retrieve(Integer(istr_mant.argumento[2]),li_grupo,-1, &
			                                   -1) = 0 THEN	
						IF idwc_color_fondo.Retrieve(Integer(istr_mant.argumento[2]),-1,-1, &
				                                   -1) = 0 THEN	
							idwc_color_fondo.InsertRow(0)
						END IF	
					END IF	
				END IF
			END IF

			IF idwc_color_fondo1.Retrieve(Integer(istr_mant.argumento[2]),li_grupo,li_subgrupo, &
			                             Integer(istr_mant.argumento[4])) = 0 THEN
				IF idwc_color_fondo1.Retrieve(Integer(istr_mant.argumento[2]),li_grupo,li_subgrupo, &
			                                -1) = 0 THEN	
					IF idwc_color_fondo1.Retrieve(Integer(istr_mant.argumento[2]),li_grupo,-1, &
			                                   -1) = 0 THEN	
						IF idwc_color_fondo1.Retrieve(Integer(istr_mant.argumento[2]),-1,-1, &
			                                   -1) = 0 THEN	
							idwc_color_fondo1.InsertRow(0)
						END IF	
					END IF	
				END IF
			END IF
	
			IF ll_Filas = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", &
								"No es posible conectar la Base de Datos.", &
								Information!, RetryCancel!)
			ELSE
			
				IF dw_3.Retrieve(Integer(istr_mant.argumento[1]),&
									  Integer(istr_mant.argumento[2]),&
									  Integer(istr_mant.argumento[3]))= -1 OR &
					dw_4.Retrieve(Integer(istr_mant.argumento[1]), &
									  Integer(istr_mant.argumento[2]), &
									  Integer(istr_mant.argumento[3]))= -1 OR &									  
					dw_5.Retrieve(Integer(istr_mant.argumento[1]),&
									  Integer(istr_mant.argumento[2]),&
									  Integer(istr_mant.argumento[3]),&
									  Integer(istr_mant.argumento[4]))= -1 OR &
					dw_6.Retrieve(Integer(istr_mant.argumento[1]),&
									  Integer(istr_mant.argumento[2]),&
									  Integer(istr_mant.argumento[3]),&
									  Integer(istr_mant.argumento[4]))= -1 OR &
					dw_7.Retrieve(Integer(istr_mant.argumento[1]),&
									  Integer(istr_mant.argumento[2]),&
									  Integer(istr_mant.argumento[3]),&
									  Integer(istr_mant.argumento[4])) = -1 OR &
					dw_9.Retrieve(Integer(istr_mant.argumento[1]),&
									  Integer(istr_mant.argumento[2]),&
									  Integer(istr_mant.argumento[3]),&
									  Integer(istr_mant.argumento[4])) = -1 OR &
					dw_8.Retrieve(Integer(istr_mant.argumento[1]),&
									  Integer(istr_mant.argumento[2]),&
									  Integer(istr_mant.argumento[3])) = -1 THEN
					respuesta = MessageBox(	"Error en Base de Datos", &
									"No es posible conectar la Base de Datos.", &
									Information!, RetryCancel!)
				ELSE
					pb_eliminar.Enabled  = TRUE
					pb_grabar.Enabled		= TRUE
					pb_imprimir.Enabled	= TRUE
					pb_ins_det.Enabled	= TRUE
					pb_eli_det.Enabled	= TRUE
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
	
				END IF
			END IF	
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_spro_lotefrutagranel.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_maed_spro_lotefrutagranel.destroy
call super::destroy
destroy(this.tab_1)
end on

event resize;//
end event

event ue_nuevo();call super::ue_nuevo;dw_2.SetItem(1, "lote_pltcod", gstr_paramplanta.codigoplanta)
dw_2.SetItem(1, "lote_espcod", gstr_paramplanta.codigoespecie)

Istr_Mant.Argumento[1] = String(gstr_paramplanta.codigoplanta)
Istr_Mant.Argumento[2] = String(gstr_paramplanta.codigoespecie)

HabilitaColumnas(False)
HabilitaDetalle(False)		
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
dw_8.REset()
dw_9.Reset()
end event

event close;call super::close;//
end event

event ue_antesguardar();call super::ue_antesguardar;Long			ll_Fila, ll_Maxsecuen, ll_secuenco
Integer		li_Planta, li_Lote, li_Especie, li_Variedad, li_Contador_suma
Decimal{2}	ld_Porcent, ld_PorcColor, ld_PorcCubrim, ld_PorcCalib
String		ls_Mensaje_suma, ls_mensaje_valor, ls_grupcal, ls_grupcalante

li_Planta		=	dw_2.Object.lote_pltcod[1]
li_Especie		=	dw_2.Object.lote_espcod[1]
li_Lote			=	dw_2.Object.lote_codigo[1]
li_Variedad		=	dw_2.Object.vari_codigo[1]


IF dw_3.RowCount()>0 THEN
	dw_3.SetRedraw(false)
	dw_3.SetSort("mcor_grucal A")
	dw_3.Sort()
	dw_3.SetRedraw(TRUE)
	
	ls_grupcalante=dw_3.Object.mcor_grucal[1]	
	
	SELECT	IsNull(Max(mcor_secuen), 0) + 1
 	  INTO	:ll_maxsecuen
  	  From dba.spro_madurezcosechare
	 Where lote_pltcod = :li_planta
		And   lote_espcod = :li_especie
		And   lote_codigo = :li_lote
		AND   mcor_grucal = :ls_grupcalante;
		//And   vari_codigo = :li_variedad ya no va
		
	FOR ll_Fila = 1 TO dw_3.RowCount()
		IF dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		  	ls_grupcal=dw_3.Object.mcor_grucal[ll_Fila]	
		  	IF ls_grupcalante=ls_grupcal THEN  
				dw_3.Object.lote_pltcod[ll_Fila]	=	li_Planta
				dw_3.Object.lote_espcod[ll_Fila]	=	li_Especie
				dw_3.Object.lote_codigo[ll_Fila]	=	li_Lote
				//dw_3.Object.vari_codigo[ll_Fila]	=	li_Variedad
				IF isnull(dw_3.Object.cofo_codigo[ll_Fila]) THEN
					dw_3.Object.mcor_secuen[ll_Fila] =	0
				ELSE	
					dw_3.Object.mcor_secuen[ll_Fila] =	ll_Maxsecuen
				END IF	
				ll_Maxsecuen ++
			ELSE
				SELECT	IsNull(Max(mcor_secuen), 0) + 1
				  INTO	:ll_maxsecuen
				  From dba.spro_madurezcosechare
				 Where lote_pltcod = :li_planta
					And   lote_espcod = :li_especie
					And   lote_codigo = :li_lote
					AND   mcor_grucal = :ls_grupcal;
					
					//And   vari_codigo = :li_variedad ya no va
					
				IF dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN	
					dw_3.Object.lote_pltcod[ll_Fila]	=	li_Planta
					dw_3.Object.lote_espcod[ll_Fila]	=	li_Especie
					dw_3.Object.lote_codigo[ll_Fila]	=	li_Lote
					//dw_3.Object.vari_codigo[ll_Fila]	=	li_Variedad
					IF isnull(dw_3.Object.cofo_codigo[ll_Fila]) THEN
						dw_3.Object.mcor_secuen[ll_Fila] =	0
					ELSE	
						dw_3.Object.mcor_secuen[ll_Fila] =	ll_Maxsecuen
					END IF	
					ll_Maxsecuen ++
					ls_grupcalante=ls_grupcal
				END IF	
			END IF
		END IF 
	NEXT
END IF

FOR ll_Fila = 1 TO dw_4.RowCount()
	
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_4.Object.lote_pltcod[ll_Fila]	=	li_Planta
		dw_4.Object.lote_espcod[ll_Fila]	=	li_Especie
		dw_4.Object.lote_codigo[ll_Fila]	=	li_Lote
	END IF
NEXT

FOR ll_Fila = 1 TO dw_5.RowCount()
	
	IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_5.Object.lote_pltcod[ll_Fila]	=	li_Planta
		dw_5.Object.lote_espcod[ll_Fila]	=	li_Especie
		dw_5.Object.lote_codigo[ll_Fila]	=	li_Lote
	END IF
NEXT

FOR ll_Fila = 1 TO dw_6.RowCount()
	
	IF dw_6.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_6.Object.lote_pltcod[ll_Fila]	=	li_Planta
		dw_6.Object.lote_espcod[ll_Fila]	=	li_Especie
		dw_6.Object.lote_codigo[ll_Fila]	=	li_Lote
	END IF
NEXT

FOR ll_Fila = 1 TO dw_7.RowCount()
	
	IF dw_7.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		IF dw_7.Object.dade_tipodd[ll_Fila] > 2 THEN
			dw_7.SetItemStatus(ll_Fila,0,Primary!,NotModified!)
		ELSE
			dw_7.Object.lote_pltcod[ll_Fila]	=	li_Planta
			dw_7.Object.lote_espcod[ll_Fila]	=	li_Especie
			dw_7.Object.lote_codigo[ll_Fila]	=	li_Lote
		END IF
	END IF
NEXT

FOR ll_Fila = 1 TO dw_9.RowCount()
	
	IF dw_9.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_9.Object.lote_pltcod[ll_Fila]	=	li_Planta
		dw_9.Object.lote_espcod[ll_Fila]	=	li_Especie
		dw_9.Object.lote_codigo[ll_Fila]	=	li_Lote
	END IF
NEXT

FOR ll_Fila = 1 TO dw_4.RowCount()
	ld_PorcColor	= ld_PorcColor + dw_4.Object.cfre_porcol[ll_Fila]
NEXT

FOR ll_Fila = 1 TO dw_5.RowCount()
	ld_PorcCubrim	= ld_PorcCubrim + dw_5.Object.ccca_porcen[ll_Fila]
NEXT

FOR ll_Fila = 1 TO dw_6.RowCount()
	ld_PorcCalib	= ld_PorcCalib + dw_6.Object.disca_porren[ll_Fila]
NEXT

IF ld_PorcColor <> 100 THEN
	li_Contador_suma ++
	ls_Mensaje_suma			+= "~nColor de Fondo"
END IF

IF ld_PorcCubrim <> 100 THEN
	li_Contador_suma ++
	ls_Mensaje_suma			+= "~nColor de Cubrimiento"
END IF

IF ld_PorcCalib <> 100 THEN
	li_Contador_suma ++
	ls_Mensaje_suma			+= "~nDistribución de Calibre"
END IF

IF li_Contador_suma > 0 THEN
	MessageBox("Control de Calidad", "La Suma de Porcentajes de :" + ls_Mensaje_suma + &
					"~rdebe ser igual a 100 %", StopSign!, Ok!)
	Message.DoubleParm = -1
END IF
end event

event ue_seleccion();call super::ue_seleccion;str_busqueda	lstr_busq
String	ls_Null

lstr_busq.argum[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
lstr_busq.argum[2]	=	istr_mant.argumento[2]

OpenWithParm(w_busc_spro_lotefrutagranel, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] = "" THEN
	dw_2.SetColumn("lote_codigo")
	HabilitaColumnas(False)
	HabilitaDetalle(False)
	dw_2.SetFocus()
ELSE
	istr_mant.Argumento[1]	=	lstr_busq.argum[1]
	istr_mant.argumento[2]	=	lstr_busq.argum[2]
	istr_mant.argumento[3]	=	lstr_busq.argum[3]
	
	dw_2.GetChild("vari_codigo", idwc_variedad)
	idwc_variedad.SetTransObject(SqlCa)
	IF idwc_variedad.Retrieve(Integer(istr_mant.argumento[2])) = 0 THEN
		MessageBox("Atención","Falta Registrar Variedades")
		idwc_variedad.InsertRow(0)
	END IF

	TriggerEvent("ue_recuperadatos")
	HabilitaDetalle(True)		
	GeneraDetalle()
	Existedano()
END IF

RETURN
end event

event ue_borrar();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
IF dw_4.RowCount() > 0 THEN dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)
IF dw_5.RowCount() > 0 THEN dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)
IF dw_6.RowCount() > 0 THEN dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)
IF dw_7.RowCount() > 0 THEN dw_7.RowsMove(1,dw_7.RowCount(),Primary!,dw_7,1,Delete!)
IF dw_9.RowCount() > 0 THEN dw_9.RowsMove(1,dw_9.RowCount(),Primary!,dw_9,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
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

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_lotefrutagranel
boolean visible = false
integer x = 91
integer y = 960
integer width = 1061
integer height = 928
boolean enabled = false
string title = ""
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_lotefrutagranel
integer x = 69
integer y = 56
integer width = 3077
integer height = 860
integer taborder = 10
string dataobject = "dw_mant_spro_lotesfrutagranel"
end type

event dw_2::itemchanged;String	ls_Null
String	ls_columna

ls_columna = GetColumnName()
SetNull(ls_Null)

CHOOSE CASE ls_columna

   CASE "lote_espcod"
			IF Existeespecie(data) THEN 
				istr_mant.argumento[2] = data
				This.GetChild("vari_codigo", idwc_variedad)
				idwc_variedad.SetTransObject(SqlCa)
				IF idwc_variedad.Retrieve(Integer(data)) = 0 THEN
					MessageBox("Atención","Falta Registrar Variedades")
					idwc_variedad.InsertRow(0)
				END IF
			ELSE	
				MessageBox(	"Error ", "No Existe Especie, Ingrese otra.", &
											Information!, OK!)
				RETURN 1
			END IF

	CASE "lote_codigo"
		IF iuo_Lotes.Existe(gstr_ParamPlanta.CodigoPlanta,&
									Integer(istr_mant.argumento[2]),&
									Integer(data), True, Sqlca) THEN
			istr_mant.argumento[3]	=	data
			HabilitaDetalle(True)
			Parent.TriggerEvent("ue_recuperadatos")
			GeneraDetalle()
			Existedano()
		ELSE
			This.SetItem(il_fila, "lote_codigo", Integer(ls_Null))
			HabilitaColumnas(False)
			HabilitaDetalle(False)
			RETURN 1
		END IF

	CASE "cocc_codigo"
		IF NOT iuo_CondiCC.Existe(Integer(data), True, Sqlca) THEN
			This.SetItem(il_fila, "cocc_codigo", Integer(ls_Null))
			RETURN 1
		END IF

	CASE "cate_codigo"
		IF Not ExisteCategoria(Integer(data), istr_categoria) THEN
			This.SetItem(il_fila, "cate_codigo", Integer(ls_Null))
			RETURN 1
		END IF
		
END CHOOSE

end event

event dw_2::doubleclicked;//
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "busca_productor"
		BuscaProductor()

END CHOOSE

end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_lotefrutagranel
integer x = 3287
integer taborder = 40
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(TRUE)
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_lotefrutagranel
integer x = 3287
integer taborder = 50
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_lotefrutagranel
integer x = 3287
integer y = 632
integer taborder = 60
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_lotefrutagranel
integer x = 3287
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_lotefrutagranel
integer x = 3287
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_lotefrutagranel
boolean visible = false
integer x = 3287
integer y = 1504
integer taborder = 0
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_lotefrutagranel
boolean visible = false
integer x = 3287
integer y = 1680
integer taborder = 0
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_lotefrutagranel
integer x = 3287
integer taborder = 30
end type

type tab_1 from tab within w_maed_spro_lotefrutagranel
event create ( )
event destroy ( )
integer x = 59
integer y = 928
integer width = 3077
integer height = 1012
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean multiline = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean powertips = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
tp_4 tp_4
tp_5 tp_5
tp_5a tp_5a
tp_6 tp_6
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.tp_3=create tp_3
this.tp_4=create tp_4
this.tp_5=create tp_5
this.tp_5a=create tp_5a
this.tp_6=create tp_6
this.Control[]={this.tp_1,&
this.tp_2,&
this.tp_3,&
this.tp_4,&
this.tp_5,&
this.tp_5a,&
this.tp_6}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
destroy(this.tp_3)
destroy(this.tp_4)
destroy(this.tp_5)
destroy(this.tp_5a)
destroy(this.tp_6)
end on

type tp_1 from userobject within tab_1
event create ( )
event destroy ( )
string tag = "Antecedentes de Madurez de Cosecha"
integer x = 18
integer y = 208
integer width = 3040
integer height = 788
boolean enabled = false
long backcolor = 12632256
string text = "Madurez Cosecha"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Environment!"
long picturemaskcolor = 553648127
dw_madurez dw_madurez
pb_inserta_mc pb_inserta_mc
pb_elimina_mc pb_elimina_mc
end type

on tp_1.create
this.dw_madurez=create dw_madurez
this.pb_inserta_mc=create pb_inserta_mc
this.pb_elimina_mc=create pb_elimina_mc
this.Control[]={this.dw_madurez,&
this.pb_inserta_mc,&
this.pb_elimina_mc}
end on

on tp_1.destroy
destroy(this.dw_madurez)
destroy(this.pb_inserta_mc)
destroy(this.pb_elimina_mc)
end on

type dw_madurez from datawindow within tp_1
integer x = 78
integer y = 48
integer width = 2693
integer height = 700
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_mues_spro_madurezcosechare"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF Row > 0 THEN
	This.SelectRow(0,False)
	This.SetRow(row)
	This.SelectRow(row,True)
END IF
end event

event doubleclicked;str_mant	lstr_mant

IF row > 0 THEN
	lstr_mant.agrega	=	False
	lstr_mant.borra	=	False
	lstr_mant.dw		=	This

	lstr_mant.argumento[1]	=	istr_mant.argumento[1]
	lstr_mant.argumento[2]	=	istr_mant.argumento[2]
	lstr_mant.argumento[3]	=	istr_mant.argumento[3]
	lstr_mant.argumento[4]	=	istr_mant.argumento[4]

	OpenWithParm(w_mant_deta_spro_madurezcosechare, lstr_mant)
END IF
end event

type pb_inserta_mc from picturebutton within tp_1
integer x = 2830
integer y = 272
integer width = 155
integer height = 132
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\inserte.bmp"
string disabledname = "\desarrollo\bmp\insertd.bmp"
alignment htextalign = left!
end type

event clicked;str_mant	lstr_mant
Long		ll_fila

lstr_mant.borra	=	False
lstr_mant.agrega	=	True
lstr_mant.dw		=	dw_madurez

lstr_mant.argumento[1]	=	istr_mant.argumento[1]
lstr_mant.argumento[2]	=	istr_mant.argumento[2]
lstr_mant.argumento[3]	=	istr_mant.argumento[3]
lstr_mant.argumento[4]	=	istr_mant.argumento[4]

OpenWithParm(w_mant_deta_spro_madurezcosechare, lstr_mant)

IF dw_madurez.RowCount() > 0 AND pb_elimina_mc.Enabled	= FALSE THEN
	pb_elimina_mc.Enabled	=	TRUE
	pb_grabar.Enabled			=	TRUE
END IF

ll_fila	=	dw_madurez.GetRow()
dw_madurez.SetRow(ll_fila)
dw_madurez.SelectRow(ll_fila, True)
end event

type pb_elimina_mc from picturebutton within tp_1
integer x = 2830
integer y = 480
integer width = 155
integer height = 132
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\suprime.bmp"
string disabledname = "\desarrollo\bmp\suprimd.bmp"
alignment htextalign = left!
end type

event clicked;str_mant	lstr_mant
Long	ll_fila

IF dw_madurez.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

Message.DoubleParm	= 0

lstr_mant.borra	=	True
lstr_mant.agrega	=	False
lstr_mant.dw		=	dw_madurez


lstr_mant.argumento[1]	=	istr_mant.argumento[1]
lstr_mant.argumento[2]	=	istr_mant.argumento[2]
lstr_mant.argumento[3]	=	istr_mant.argumento[3]
lstr_mant.argumento[4]	=	istr_mant.argumento[4]

OpenWithParm(w_mant_deta_spro_madurezcosechare, lstr_mant)

lstr_mant	= Message.PowerObjectParm

IF lstr_mant.respuesta	= 1 THEN
	ll_fila	= dw_madurez.GetRow()
	IF dw_madurez.DeleteRow(ll_fila)=1 THEN
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		MessageBox("Madurez Cosecha", "No se puede borrar actual Registro.")
	END IF
	
	IF dw_madurez.RowCount() = 0 THEN
		pb_elimina_mc.Enabled	= False
	ELSE
		dw_madurez.SelectRow(ll_fila, True)
	END IF
END IF
end event

type tp_2 from userobject within tab_1
event create ( )
event destroy ( )
string tag = "Antecedentes de Color de Fondo"
integer x = 18
integer y = 208
integer width = 3040
integer height = 788
boolean enabled = false
long backcolor = 12632256
string text = "Color Fondo"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "UserObject!"
long picturemaskcolor = 553648127
dw_colorfondo dw_colorfondo
end type

on tp_2.create
this.dw_colorfondo=create dw_colorfondo
this.Control[]={this.dw_colorfondo}
end on

on tp_2.destroy
destroy(this.dw_colorfondo)
end on

type dw_colorfondo from datawindow within tp_2
integer x = 78
integer y = 48
integer width = 2693
integer height = 700
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_mues_spro_colordefondorec"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_columna, ls_Null
Integer	li_color, li_Null, li_grupo, li_subgrupo
Long		ll_FilaDano
Dec{2}	ld_SumaPorc, ld_PorcAnt

SetNull(ls_Null)
SetNull(li_Null)

ls_columna = dwo.Name

CHOOSE CASE ls_columna
//	CASE "cofo_codigo"
//		IF Not iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), &
//												Integer(istr_Mant.Argumento[4]), &
//												Integer(Data), False, Sqlca) OR &
//			DuplicadoColor(data, 1) THEN
//			This.SetItem(il_fila, "cofo_codigo", Integer(ls_Null))
//			RETURN 1
//		ELSE
//			This.SetItem(il_fila, "cofo_nombre", iuo_ColorFondo.Nombre)
//		END IF
		
	CASE "cfre_porcol"
		ld_PorcAnt	=	This.Object.cfre_porcol[row]
		ld_SumaPorc	=	This.Object.total_porcen[row]
		
		IF Dec(data) < 0 THEN
			This.SetItem(row, "cfre_porcol", Dec(ls_Null))
			RETURN 1
		END IF
		
		IF Dec(data) > 100 THEN
			MessageBox("Atención","Distribución de Calibre no puede sobrepasar el 100 %")
			This.SetItem(il_fila, "cfre_porcol", ld_PorcAnt)
			RETURN 1
		ELSE
			IF ld_SumaPorc + Dec(data) - ld_PorcAnt > 100 THEN
				MessageBox("Atención","Suma de Porcentaje no puede sobrepasar el 100 %")
				This.SetItem(il_fila, "cfre_porcol", ld_PorcAnt)
				RETURN 1
			END IF
		END IF
		
		li_color	=	This.Object.cofo_codigo[row]
		IF iuo_variedad.existe(Integer(istr_Mant.Argumento[2]),Integer(istr_Mant.Argumento[4]),&
	   	                    TRUE,SQLCA) THEN
			li_grupo 	= 	iuo_variedad.grupo
			li_subgrupo	=	iuo_variedad.subgrupo
		ELSE
			SetNull(li_grupo)
			SetNull(li_subgrupo)
		END IF 
		
		IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), li_grupo, li_subgrupo, &
												Integer(istr_Mant.Argumento[4]), &
												li_color, False, Sqlca) THEN
			IF iuo_ColorFondo.Dano = 1 THEN
				ll_FilaDano	=	dw_7.Find("dade_tipodd = 3 and " + &
												 "dade_codigo = 0 and " + &
												 "dano_altern = '"+String(li_Color)+"'",1,dw_7.RowCount())
				IF ll_FilaDano = 0 THEN
					ll_FilaDano	=	dw_7.InsertRow(0)
					
					dw_7.SetItem(ll_FilaDano,"dade_tipodd",3)
					dw_7.SetItem(ll_FilaDano,"dade_codigo",0)
					dw_7.SetItem(ll_FilaDano,"dano_altern",String(li_Color))
					dw_7.SetItem(ll_FilaDano,"dade_nombre",iuo_ColorFondo.Nombre)
					dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
				ELSE
					IF Dec(data) = 0 THEN
						dw_7.DeleteRow(ll_FilaDano)
					ELSE
						dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
					END IF
				END IF
			END IF
		ELSE
			IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), li_grupo, li_subgrupo, &
												-1, li_color, False, Sqlca) THEN
				IF iuo_ColorFondo.Dano = 1 THEN
					ll_FilaDano	=	dw_7.Find("dade_tipodd = 3 and " + &
													 "dade_codigo = 0 and " + &
													 "dano_altern = '"+String(li_Color)+"'",1,dw_7.RowCount())
					IF ll_FilaDano = 0 THEN
						ll_FilaDano	=	dw_7.InsertRow(0)
						
						dw_7.SetItem(ll_FilaDano,"dade_tipodd",3)
						dw_7.SetItem(ll_FilaDano,"dade_codigo",0)
						dw_7.SetItem(ll_FilaDano,"dano_altern",String(li_Color))
						dw_7.SetItem(ll_FilaDano,"dade_nombre",iuo_ColorFondo.Nombre)
						dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
					ELSE
						IF Dec(data) = 0 THEN
							dw_7.DeleteRow(ll_FilaDano)
						ELSE
							dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
						END IF
					END IF
				END IF
			ELSE
				IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), li_grupo, -1, &
												-1, li_color, False, Sqlca) THEN
					IF iuo_ColorFondo.Dano = 1 THEN
						ll_FilaDano	=	dw_7.Find("dade_tipodd = 3 and " + &
														 "dade_codigo = 0 and " + &
														 "dano_altern = '"+String(li_Color)+"'",1,dw_7.RowCount())
						IF ll_FilaDano = 0 THEN
							ll_FilaDano	=	dw_7.InsertRow(0)
							
							dw_7.SetItem(ll_FilaDano,"dade_tipodd",3)
							dw_7.SetItem(ll_FilaDano,"dade_codigo",0)
							dw_7.SetItem(ll_FilaDano,"dano_altern",String(li_Color))
							dw_7.SetItem(ll_FilaDano,"dade_nombre",iuo_ColorFondo.Nombre)
							dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
						ELSE
							IF Dec(data) = 0 THEN
								dw_7.DeleteRow(ll_FilaDano)
							ELSE
								dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
							END IF
						END IF
					END IF
				ELSE
					IF iuo_ColorFondo.Existe(Integer(istr_Mant.Argumento[2]), -1, -1, &
												-1, li_color, False, Sqlca) THEN
						IF iuo_ColorFondo.Dano = 1 THEN
							ll_FilaDano	=	dw_7.Find("dade_tipodd = 3 and " + &
															 "dade_codigo = 0 and " + &
															 "dano_altern = '"+String(li_Color)+"'",1,dw_7.RowCount())
							IF ll_FilaDano = 0 THEN
								ll_FilaDano	=	dw_7.InsertRow(0)
								
								dw_7.SetItem(ll_FilaDano,"dade_tipodd",3)
								dw_7.SetItem(ll_FilaDano,"dade_codigo",0)
								dw_7.SetItem(ll_FilaDano,"dano_altern",String(li_Color))
								dw_7.SetItem(ll_FilaDano,"dade_nombre",iuo_ColorFondo.Nombre)
								dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
							ELSE
								IF Dec(data) = 0 THEN
									dw_7.DeleteRow(ll_FilaDano)
								ELSE
									dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
								END IF
							END IF
						END IF	
					END IF
				END IF
			END IF
		END IF
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

event losefocus;Accepttext()
end event

event clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

type tp_3 from userobject within tab_1
event create ( )
event destroy ( )
string tag = "Color de Cubrimiento por Categoría"
integer x = 18
integer y = 208
integer width = 3040
integer height = 788
boolean enabled = false
long backcolor = 12632256
string text = "Color Cubrimiento"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Custom072!"
long picturemaskcolor = 553648127
dw_cubrim dw_cubrim
end type

on tp_3.create
this.dw_cubrim=create dw_cubrim
this.Control[]={this.dw_cubrim}
end on

on tp_3.destroy
destroy(this.dw_cubrim)
end on

type dw_cubrim from datawindow within tp_3
integer x = 78
integer y = 48
integer width = 2693
integer height = 700
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_mues_spro_colcubcategoriaare"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemerror;RETURN 1
end event

event itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;Accepttext()
end event

event rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event itemchanged;Integer	li_Categoria, li_Nula
Long		ll_FilaDano
Dec{2}	ld_SumaPorc, ld_PorcAnt

SetNull(li_Nula)

CHOOSE CASE dwo.Name
	CASE "ccca_porcen"
		ld_PorcAnt	=	This.Object.ccca_porcen[row]
		ld_SumaPorc	=	This.Object.total_porcen[row]
		
		IF Dec(data) < 0 THEN
			This.SetItem(row, "ccca_porcen", Dec(li_Nula))
			RETURN 1
		END IF
		
		IF Dec(data) > 100 THEN
			MessageBox("Atención","Color de Cubrimiento no puede sobrepasar el 100 %")
			This.SetItem(il_fila, "ccca_porcen", ld_PorcAnt)
			RETURN 1
		ELSE
			IF ld_SumaPorc + Dec(data) - ld_PorcAnt > 100 THEN
				MessageBox("Atención","Suma de Porcentaje no puede sobrepasar el 100 %")
				This.SetItem(il_fila, "ccca_porcen", ld_PorcAnt)
				RETURN 1
			END IF
		END IF

		li_Categoria	=	This.Object.cate_codigo[row]
		
		IF Not iuo_EspecieCatego.Existe(Integer(istr_Mant.Argumento[2]), &
												Integer(istr_Mant.Argumento[4]), &
												li_Categoria, False, Sqlca) THEN
			iuo_EspecieCatego.Existe(Integer(istr_Mant.Argumento[2]), &
										li_nula,li_Categoria, False, Sqlca)
		END IF
		IF iuo_EspecieCatego.Dano = 1 THEN
			ll_FilaDano	=	dw_7.Find("dade_tipodd = 4 and " + &
											 "dade_codigo = 0 and " + &
											 "dano_altern = '"+String(li_Categoria)+"'",1,dw_7.RowCount())
			IF ll_FilaDano = 0 THEN
				ll_FilaDano	=	dw_7.InsertRow(0)
				
				dw_7.SetItem(ll_FilaDano,"dade_tipodd",4)
				dw_7.SetItem(ll_FilaDano,"dade_codigo",0)
				dw_7.SetItem(ll_FilaDano,"dano_altern",String(li_Categoria))
				dw_7.SetItem(ll_FilaDano,"dade_nombre",iuo_EspecieCatego.Nombre)
				dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
			ELSE
				IF Dec(data) = 0 THEN
					dw_7.DeleteRow(ll_FilaDano)
				ELSE
					dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
				END IF
			END IF
		END IF
END CHOOSE
end event

type tp_4 from userobject within tab_1
event create ( )
event destroy ( )
string tag = "Distribución por Calibre"
integer x = 18
integer y = 208
integer width = 3040
integer height = 788
boolean enabled = false
long backcolor = 12632256
string text = "Distribución Calibre"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "ArrangeIcons!"
long picturemaskcolor = 553648127
dw_calibre dw_calibre
end type

on tp_4.create
this.dw_calibre=create dw_calibre
this.Control[]={this.dw_calibre}
end on

on tp_4.destroy
destroy(this.dw_calibre)
end on

type dw_calibre from datawindow within tp_4
integer x = 78
integer y = 48
integer width = 2693
integer height = 700
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_mues_spro_distribcalibresre"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemerror;RETURN 1
end event

event itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF

String	ls_columna, ls_Null

SetNull(ls_Null)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna
	CASE "disca_grupca"
		
//		IF	DuplicadoCalibre( data, 1) THEN
//			This.SetItem(il_fila, "disca_grupca", Integer(ls_Null))
//			RETURN 1
//		END IF

END CHOOSE
end event

event losefocus;Accepttext()
end event

event rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event itemchanged;String	ls_columna, ls_Null, ls_GruCal
Long		ll_FilaDano
Dec{2}	ld_SumaPorc,ld_PorcAnt

SetNull(ls_Null)

ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "disca_porren"
		ld_PorcAnt	=	This.Object.disca_porren[row]
		ld_SumaPorc	=	This.Object.total_porcen[row]
		
		IF Dec(data) < 0 THEN
			This.SetItem(row, "disca_porren", Dec(ls_Null))
			RETURN 1
		END IF
		
		IF Dec(data) > 100 THEN
			MessageBox("Atención","Distribución de Calibre no puede sobrepasar el 100 %")
			This.SetItem(il_fila, "disca_porren", ld_PorcAnt)
			RETURN 1
		ELSE
			IF ld_SumaPorc + Dec(data) - ld_PorcAnt > 100 THEN
				MessageBox("Atención","Suma de Porcentaje no puede sobrepasar el 100 %")
				This.SetItem(il_fila, "disca_porren", ld_PorcAnt)
				RETURN 1
			END IF
		END IF
		
		ls_GruCal	=	This.Object.disca_grupca[row]
		
		IF iuo_EspeVariGruCal.Existe(Integer(istr_Mant.Argumento[2]), &
												Integer(istr_Mant.Argumento[4]), &
												ls_GruCal, False, Sqlca) THEN
			IF iuo_EspeVariGruCal.Dano = 1 THEN
				ll_FilaDano	=	dw_7.Find("dade_tipodd = 5 and " + &
												 "dade_codigo = 0 and " + &
												 "dano_altern = '"+ ls_GruCal +"'",1,dw_7.RowCount())
				IF ll_FilaDano = 0 THEN
					ll_FilaDano	=	dw_7.InsertRow(0)
					
					dw_7.SetItem(ll_FilaDano,"dade_tipodd",5)
					dw_7.SetItem(ll_FilaDano,"dade_codigo",0)
					dw_7.SetItem(ll_FilaDano,"dano_altern",ls_GruCal)
					dw_7.SetItem(ll_FilaDano,"dade_nombre",iuo_EspeVariGruCal.Nombre)
					dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
				ELSE
					IF Dec(data) = 0 THEN
						dw_7.DeleteRow(ll_FilaDano)
					ELSE
						dw_7.SetItem(ll_FilaDano,"dade_podade",Dec(data))
					END IF
				END IF
			END IF
		END IF
		
END CHOOSE
end event

type tp_5 from userobject within tab_1
event create ( )
event destroy ( )
string tag = "Daños y Defectos detectados"
integer x = 18
integer y = 208
integer width = 3040
integer height = 788
boolean enabled = false
long backcolor = 12632256
string text = "Daños y Defectos"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Custom066!"
long picturemaskcolor = 536870912
dw_danos dw_danos
end type

on tp_5.create
this.dw_danos=create dw_danos
this.Control[]={this.dw_danos}
end on

on tp_5.destroy
destroy(this.dw_danos)
end on

type dw_danos from datawindow within tp_5
integer x = 78
integer y = 48
integer width = 2693
integer height = 700
integer taborder = 10
string title = "none"
string dataobject = "dw_mues_spro_danoydefectosre"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event itemerror;RETURN 1
end event

event itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;Accepttext()
end event

event rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event itemchanged;Integer	li_TipoDano
String	ls_Codigo, ls_Null
Long		ll_Fila

dw_7.AcceptText()

CHOOSE CASE dwo.Name
	CASE "dade_tipodd"
		idwc_danos.Retrieve(Integer(istr_mant.Argumento[2]),Integer(data))
		
	CASE "dade_codigo"
		This.SetItem(row,"dade_nombre",idwc_danos.GetItemString(idwc_danos.GetRow(),"dade_nombre"))
		
	CASE "dade_podade"
		IF Dec(data) >= 100 OR Dec(data) < 0 THEN
			This.SetItem(row,"dade_podade", Dec(ls_Null))
			RETURN 1
		END IF
		
		li_TipoDano	=	This.Object.dade_tipodd[row]
		
		IF li_TipoDano > 2 THEN
			MessageBox("Atención","Los Daños Color de Fondo, Cubrimiento y Calibre deben "+&
							"ser modificados en su origen")
							
			ls_Codigo	=	This.Object.dano_altern[row]
			CHOOSE CASE li_TipoDano
				CASE 3
					ll_Fila	=	dw_4.Find("cofo_codigo = "+ls_Codigo,1,dw_4.RowCount())
					
					This.SetItem(row,"dade_podade",dw_4.Object.cfre_porcol[ll_Fila])
					
				CASE 4
					
					ll_Fila	=	dw_5.Find("cate_codigo = "+ls_Codigo,1,dw_5.RowCount())

					This.SetItem(row,"dade_podade",dw_5.Object.ccca_porcen[ll_Fila])
				
				CASE 5

					ll_Fila	=	dw_6.Find("disca_grupca ='"+ls_Codigo+"'",1,dw_6.RowCount())

					This.SetItem(row,"dade_podade",dw_6.Object.disca_porren[ll_Fila])
					
			END CHOOSE
			
			RETURN 1
		END IF
		
	CASE "dade_poddpr"
		IF Dec(data) >= 100 OR Dec(data) < 0 THEN
			This.SetItem( row, "dade_poddpr", Dec(ls_Null))
			RETURN 1
		END IF
END CHOOSE
end event

type tp_5a from userobject within tab_1
integer x = 18
integer y = 208
integer width = 3040
integer height = 788
boolean enabled = false
long backcolor = 12632256
string text = "Daños Otras Categorias"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Debug5!"
long picturemaskcolor = 536870912
pb_elimina_doc pb_elimina_doc
pb_inserta_doc pb_inserta_doc
dw_otrosdanos dw_otrosdanos
end type

on tp_5a.create
this.pb_elimina_doc=create pb_elimina_doc
this.pb_inserta_doc=create pb_inserta_doc
this.dw_otrosdanos=create dw_otrosdanos
this.Control[]={this.pb_elimina_doc,&
this.pb_inserta_doc,&
this.dw_otrosdanos}
end on

on tp_5a.destroy
destroy(this.pb_elimina_doc)
destroy(this.pb_inserta_doc)
destroy(this.dw_otrosdanos)
end on

type pb_elimina_doc from picturebutton within tp_5a
integer x = 2830
integer y = 480
integer width = 155
integer height = 132
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\suprime.bmp"
string disabledname = "\desarrollo\bmp\suprimd.bmp"
alignment htextalign = left!
end type

event clicked;SetPointer(HourGlass!)

w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_9.DeleteRow(0) = 1 THEN
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		MessageBox("Atención","No se puede borrar actual registro.")
	END IF

	IF dw_9.RowCount() = 0 THEN
		This.Enabled = False
	ELSE
		il_fila = dw_9.GetRow()
	END IF
END IF
end event

type pb_inserta_doc from picturebutton within tp_5a
integer x = 2830
integer y = 272
integer width = 155
integer height = 132
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\inserte.bmp"
string disabledname = "\desarrollo\bmp\insertd.bmp"
alignment htextalign = left!
end type

event clicked;IF istr_Mant.Solo_Consulta THEN RETURN

il_fila = dw_9.InsertRow(0)

dw_9.ScrollToRow(il_fila)
dw_9.SetRow(il_fila)
dw_9.SetFocus()

dw_9.SetColumn("dade_nivcal")
end event

type dw_otrosdanos from datawindow within tp_5a
integer x = 78
integer y = 48
integer width = 2693
integer height = 700
integer taborder = 40
string title = "none"
string dataobject = "dw_mues_spro_danosdefecnivcalre"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna
Integer	li_Null, li_Tipodd, li_Nivelc, li_Codigo, li_Muestra = 1
Long		ll_Fila

SetNull(li_Null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "dade_nivcal"
		IF DuplicadoOtrosDanos(ls_Columna, data) THEN
			This.SetItem(row,ls_columna,li_Null)
			RETURN 1
		END IF
		
	CASE "dade_tipodd"
		
		IF DuplicadoOtrosDanos(ls_Columna, data) THEN
			This.SetItem(row,ls_columna,li_Null)
			RETURN 1
		ELSE
			This.SetItem(row, "dade_codigo", li_Null)
			This.SetItem(row, "dade_nombre", String(li_Null))
		END IF
		
	CASE "dade_codigo"
		IF IsNull(This.Object.dade_tipodd[row]) THEN
			Messagebox("Error de Consistencia","Debe ingresar el tipo de Daño/Defecto")
			This.SetItem(row, "dade_codigo", li_null)
			RETURN 1
		ELSE
			li_Tipodd	=	This.Object.dade_tipodd[row]
			
			IF DuplicadoOtrosDanos(ls_Columna, data) OR &
				NOT iuo_Danos.Existe(Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[4]),&
									  li_Tipodd,Integer(data),True,SQLCA) THEN
				This.SetItem(row, ls_Columna, li_Null)
				This.SetItem(row, "dade_nombre", String(li_Null))
				RETURN 1
			ELSE
				This.SetItem(row, "dade_nombre", iuo_Danos.NombreDano)
			END IF
		END IF
		
	CASE "dade_podade", "dade_poddpr"
		IF Dec(data) >= 100 OR Dec(data) < 0 THEN
			This.SetItem(row, ls_Columna, Dec(li_Null))
			RETURN 1
		END IF

		IF IsNull(This.Object.dade_tipodd[row]) OR IsNull(This.Object.dade_codigo[row]) THEN
			Messagebox("Error de Consistencia","Debe ingresar los datos que anteceden")
			This.SetItem(row, ls_columna, dec(li_Null))
			RETURN 1
		END IF
		
		li_NivelC	=	This.Object.dade_nivcal[row]
		li_Tipodd	=	This.Object.dade_tipodd[row]
		li_Codigo	=	This.Object.dade_codigo[row]
		

		IF ls_Columna = 'dade_poddpr' THEN li_Muestra = 2
		
		IF NOT AscendenciaDano(li_NivelC, li_Tipodd, li_Codigo, Dec(data), li_Muestra) THEN
			IF li_Muestra = 2 THEN
				This.SetItem(row, "dade_poddpr", li_Null)
			ELSE
				This.SetItem(row, "dade_podade", li_Null)
			END IF
			
			RETURN 1
		END IF
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

event itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;Accepttext()
end event

event rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

type tp_6 from userobject within tab_1
string tag = "Muestra Detalle del Lote"
integer x = 18
integer y = 208
integer width = 3040
integer height = 788
boolean enabled = false
long backcolor = 12632256
string text = "Detalle Lote"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Custom070!"
long picturemaskcolor = 536870912
dw_lotedeta dw_lotedeta
end type

on tp_6.create
this.dw_lotedeta=create dw_lotedeta
this.Control[]={this.dw_lotedeta}
end on

on tp_6.destroy
destroy(this.dw_lotedeta)
end on

type dw_lotedeta from datawindow within tp_6
string tag = "muestra el detalle del lote"
integer x = 78
integer y = 48
integer width = 2885
integer height = 700
integer taborder = 40
string title = "none"
string dataobject = "dw_mues_recepcion_lotedeta"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

